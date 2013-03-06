package NoSQL::PL2SQL::Simple;

use 5.008009;
use strict;
use warnings;

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use NoSQL::PL2SQL::Simple ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(
	) ] ) ;

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } ) ;

our @EXPORT = qw(
	) ;

our $VERSION = '0.21' ;

use Scalar::Util ;
use base qw( NoSQL::PL2SQL ) ;
use Carp ;

my @autodestroy = () ;

my @sql = (
	[ qw( textkey textvalue 1 ) ],
	[ qw( intkey intvalue 0 ) ],
	[ qw( datekey datevalue 1 ) ],
	) ;
my %sql = map { $_->[0] => $_ } @sql ;

my %private ;

################################################################################
##
##  update() refreshes the instance after data definition changes
##
################################################################################

$private{update} = sub {
	my $self = shift ;
	my $tied = tied %$self ;
	my $package = ref $self ;

	return unless $tied->{tied} ;
	delete $tied->{tied} ;

	my $o = $package->SQLObject( $tied->{dsn}->{object}, 0 ) ;
	my %keys = map { $_ => &{ $private{recno} }( $o, $_ ) } keys %$o ;
	$tied->{keys} = \%keys ;
	} ;


################################################################################
##
##  sqlsave() rolls back the rollback
##
################################################################################

$private{sqlsave} = sub {
	my $self = shift ;
	my $tied = tied %$self ;
	delete $tied->{globals}->{rollback} ;
	} ;


################################################################################
##
##  recno() is part of the constructor.  It uses the internal structure
##  of NoSQL::PL2SQL::Object to pull out the unique recordID.
##
################################################################################

$private{recno} = sub {
	my $self = shift ;
	my $tied = tied %$self ;
	return $tied->record->{objectid} || $tied->{top} unless @_ ;

	my $key = shift ;
	return $tied->data->{$key}->{top} ;
	} ;


################################################################################
##
##  filter() takes two arrays and returns the intersection.  It is called 
##  recursively by query().
##
################################################################################

$private{filter} = sub {
	my @set = @{ shift @_ } ;
	return [] unless @set ;
	return [ sort { $a <=> $b } @set ] unless @_ ;

	my @against = sort { $a <=> $b } @{ shift @_ } ;
	my @out = () ;

	while ( @set && @against ) {
		my $cmp = $set[0] <=> $against[0] ;
		shift @set if $cmp < 0 ;
		shift @against if $cmp > 0 ;
		next if $cmp ;

		push @out, shift @set ;
		shift @against ;
		}

	return \@out ;
	} ;


################################################################################
##
##  index() is the common method to addTextIndex, addNumberIndex, etc.
##
################################################################################

$private{index} = sub {
	my $self = shift ;
	my $tied = tied %$self ;
	my $package = ref $self ;

	$tied->{tied} ||= $package->SQLObject( 
			$tied->{dsn}->{object}, 0 ) ;
	my $i = push @autodestroy, $tied->{tied} ;
	Scalar::Util::weaken( $autodestroy[ $i -1 ] ) ;
	return unless @_ ;

	my $type = shift ;
	map { $self->{$_} = $tied->{tied}->{$_} = $type } @_ ; 
	} ;


################################################################################
##
##  matching() is commonly called by methods to query the index database.
##
################################################################################

$private{matching} = sub {
	my $self = shift ;
	my $tied = tied %$self ;
	my $package = ref $self ;

	my $name = shift ;

	my $format = defined $name?  $sql{ $self->{ $name } }: $sql{intkey} ;

	my @sql = () ;
	push @sql, [ $format->[0], defined $name?
				$tied->{keys}->{ $name }: $tied->{id}
				] ;
	unless ( @_ ) {
		my @rows = $tied->{dsn}->{index}->fetch( @sql ) ;
		return $rows[0] unless ref $rows[0] ;
		return [] unless keys %{ $rows[0] } ;

		my @out = map { $_->{objectid} 
				=> $_->{ $format->[1] } } @rows ;
		return \@out ;
		}

	my $value = shift ;
	push @sql, [ $format->[1], $value, $format->[2] ] ;

	my @rows = $tied->{dsn}->{index}->fetch( @sql ) ;
	return $rows[0] if @rows && ! ref $rows[0] ;
	return [] unless keys %{ $rows[0] } ;

	my @out = map { $_->{objectid} => $_->{ $format->[1] } } @rows ;
	return \@out ;
	} ;


################################################################################
##
##  indexmap() creates the structures to create the SQL insert statements
##  for the index table
##
################################################################################

$private{indexmap} = sub {
	my $self = shift ;
	my $tied = tied %$self ;
	my $keys = shift ;
	my $value = shift ;
	my $orderid = shift ;
	my $format = $sql{ $self->{ $keys->[1] } } ;

	my @index = () ;
	push @index, [ $format->[0], $tied->{keys}->{ $keys->[1] } ] ;
	push @index, [ $format->[1], $value->{ $keys->[0] }, 
			$format->[2] ] ;
	push @index, [ objectid => $orderid ] ;
	return \@index ;
	} ;

################################################################################
##
##  getinstance() returns null for passed instances
##  distinguishes between instances and objects
##
################################################################################

$private{getinstance} = sub {
	my $self = shift ;
	my $tied = tied %$self ;
	return $tied->{parent} ;
	} ;

################################################################################
##
##  A tied hash is used to hide internal properties by overloading access
##  methods.
##
################################################################################

sub TIEHASH {
	my $package = shift ;
	my $self = shift ;
	return bless $self, $package ;
	}

sub CLEAR {
	my $self = shift ;
	undef $self->{clone} ;
	}

sub FETCH {
	my $self = shift ;
	my $key = shift ;
	return $self->{clone}->{$key} ;
	}

sub EXISTS {
	my $self = shift ;
	my $key = shift ;
	return exists $self->{clone}->{$key} ;
	}

sub DELETE {
	my $self = shift ;
	my $key = shift ;
	return delete $self->{clone}->{$key} ;
	}

sub STORE {
	my $self = shift ;
	my $key = shift ;
	my $value = shift ;
	return $self->{clone}->{$key} = $value ;
	}

sub FIRSTKEY {
	my $self = shift ;
	$self->{nextkey} = [ keys %{ $self->{clone} } ] ;
	return $self->NEXTKEY ;
	}

sub NEXTKEY {
	my $self = shift ;
	return shift @{ $self->{nextkey} } ;
	}

sub new {
	my $package = shift ;
	my $self = {} ;

	my @dsn = ( @_, $package->dsn ) ;
	carp( "Missing data sources" ) and return undef unless @dsn ;

	my $dsn = {} ;
	$dsn->{object} = shift @dsn ;
	$dsn->{index} = shift @dsn ;

	$package->SQLError( ObjectNotFound => \&newobject ) ;

	my $o = $package->SQLObject( $dsn->{object}, 0 ) ;
	$self->{id} = &{ $private{recno} }( $o ) ;
	$self->{clone} = $o->SQLClone() ;
	$self->{dsn} = $dsn ;
	
	my %keys = map { $_ => &{ $private{recno} }( $o, $_ ) } keys %$o ;
	$self->{keys} = \%keys ;
	tie my %out, __PACKAGE__, $self ;

	return bless \%out, $package ;
	}

sub loadschema {
	my $package = shift @_ unless ref $_[0] ;
	my ( $dsn, $index ) = $package->dsn if defined $package ;

	$dsn ||= shift @_ ;
	my $table = shift @_ if @_ && ! ref $_[0] ;
	$index ||= shift @_ if @_ ;
	$index ||= $dsn->table( $table ) if $dsn && $table ;
	carp( "Missing data sources" ) and return 
			unless defined $dsn && defined $index ;

	$dsn->loadschema ;
	$index->loadschema( $dsn->indexschema ) ;
	}

sub dsn {
	my $package = shift ;
	return () ;
	}

sub addTextIndex {
	my $self = shift ;
	return &{ $private{index} }( $self, $sql[0][0], @_ ) ;
	}

sub addNumberIndex {
	my $self = shift ;
	return &{ $private{index} }( $self, $sql[1][0], @_ ) ;
	}

sub addDateIndex {
	my $self = shift ;
	return &{ $private{index} }( $self, $sql[2][0], @_ ) ;
	}

sub recordID {
	my $array = shift ;
	my @args = @$array ;
	my $self = shift @args ;
	my @keys = keys %{ { @args } } ;
	return $keys[0] unless wantarray ;
	return @keys ;
	}

sub records {
	my $array = shift ;
	my @args = @$array ;
	my $self = shift @args ;

	my @out = map { $self->record( $_ ) } keys %{ { @args } } ;
	return $out[0] if @out && ! wantarray ;
	return @out ;
	}

sub record {
	my $self = shift ;
	return $self->records if $self->isa('ARRAY') ;

	my $tied = tied %$self ;
	my $package = ref $self ;

	return undef unless @_ ;

	&{ $private{update} }( $self ) ;

	my @args = ( shift @_ ) ;
	push @args, ( shift @_ ) if @_ && ref $_[0] ;
	push @args, undef ;
	my ( $objectid, $value ) = 
			ref $args[0]? ( undef, $args[0] ): @args[0,1] ;

	my $vv = tied %$value if $value && ref $value eq ref $self ;
	my $update = ! defined $objectid ;
	$#args = 0 if defined $objectid ;
	$objectid = $vv->{id} if defined $vv && exists $vv->{id} ;

	my $dsn = $tied->{dsn}->{object} ;
	my $index = $tied->{dsn}->{index} ;

	my $lastvalue ;
	my $out = $vv || {} ;
	my %index = @_ ;
	my @index = () ;
	if ( $value ) {
		map { push @index, [ $_ => $index{$_} ] }
				grep exists $self->{ $index{$_} }, 
				keys %index ;
		map { push @index, [ $_ => $_ ] }
				grep exists $value->{$_},
				keys %$self ;
		}

	while ( defined $value ) {
		unless ( defined $objectid ) {
			$lastvalue = $package->SQLObject( $dsn, $value ) ;
			last ;
			}

		$update = 1 ;
		$index->delete( [ objectid => $objectid ] ) ;

		my $archive = $package->SQLClone( $dsn, $objectid )
				if $self->{archive} ;

		if ( defined $args[1] ) {
			delete $out->{clone} ;
			$dsn->delete( [ objectid => $objectid ] ) ;
			$out->{clone} = $package->SQLObject( 
					$dsn, $objectid, $args[1] 
					) ;
			}
		else {
			&{ $private{sqlsave} }( $out->{clone} ) ;
			}

		last unless defined $archive ;

		my $archiveid = &{ $private{recno} }(
				$package->SQLObject( $dsn, $archive )
				) ;
		$index->insert( 
				[ intkey => $tied->{keys}->{archive} ],
				[ intvalue => $objectid ],
				[ objectid => $archiveid ]
				) ;
		last ;
		}

	delete $out->{clone} ;
	$out->{clone} = $lastvalue || $package->SQLObject( $dsn, $objectid ) ;
	return undef unless $out->{clone} ;
	$out->{clone}->SQLRollback ;
	$out->{id} = $out->{clone}->sqlobjectid ;	## lc method name
	$out->{parent} = $self ;

	map { $index->update( undef, @$_ ) }
			map { &{ $private{indexmap} }( 
			  $self, $_, $out->{clone}, $out->{id} )
			  } @index ;

	$index->update( undef,
			[ intkey => $tied->{id} ],
			[ intvalue => $out->{id} ],
			[ objectid => $out->{id} ]
			) if $update ;

	tie my %out, __PACKAGE__, $out ;
	return bless \%out, $package ;
	}

sub save {
	my $self = shift ;
	my $tied = tied %$self ;

	return $self->record( @_ ) unless $tied->{parent} ;
	return $tied->{parent}->record( $self, @_ ) ;
	}

sub reindex {
	my $self = shift ;
	my $tied = tied %$self ;
	my $parent = $tied->{parent} ;
	$tied = tied %$parent ;
	my $index = $tied->{dsn}->{index} ;
	my $objectid = $self->SQLObjectID ;

	return "reindex() requires an index name" unless @_ ;
	my $propkey = shift ;
	my $indexkey = @_? shift @_: $propkey ;

	return "unknown index: $indexkey" unless $parent->{ $indexkey } ;

	my $format = $sql{ $parent->{ $indexkey } } ;
	my $key = $tied->{keys}->{ $indexkey } ;

	$index->delete( [ $format->[0] => $key ],
			[ objectid => $objectid ] ) ;
	$index->update( undef, [ $format->[0] => $key ],
			[ $format->[1] => $self->{ $propkey }, $format->[2] ],
			[ objectid => $objectid ] ) ;
	return undef ;
	}

sub SQLObjectID {
	my $self = shift ;
	my $tied = tied %$self ;
	return $tied->{id} ;
	}

sub keyValues {
	my $self = shift ;
	my $indexid = shift ;

	my $instance = &{ $private{getinstance} }( $self ) ;
	carp "Argument is not an object" and return () unless $instance ;

	my $tied = tied %$instance ;
	my $dsn = $tied->{dsn}->{index} ;
	my $format = $sql{ $instance->{$indexid} } ;
	my @sql = ( [ $format->[0], $tied->{keys}->{$indexid} ], 
			[ objectid => $self->SQLObjectID ] ) ;

	if ( @_ == 0 ) {
		return bless [ $dsn, @sql ], __PACKAGE__ .'::keyValues'
				unless wantarray ;
		return map { $_->{ $format->[1] } } $dsn->fetch( @sql ) ;
		}

	map { $dsn->insert( @sql, [ $format->[1], $_, $format->[2] ] ) } @_ ;
	}

sub NoSQL::PL2SQL::Simple::keyValues::clear {
	my $args = shift ;
	my $dsn = shift @$args ;
	$dsn->delete( @$args ) ;
	}

sub query {
	my $self = shift ;
	my $package = ref $self ;

	my @nvp = () ;
	push @nvp, [ splice @_, 0, 2 ] while @_ ;
	push @nvp, [] unless @nvp ;

	my @error = grep @$_ && ! exists $self->{ $_->[0] }, @nvp ;
	carp sprintf( "Unknown data definition %s", $error[0][0] )
		and return () if @error ;

	my $out = &{ $private{matching} }( $self, @{ shift @nvp } ) ;
	return $out unless ref $out ;
	my @values = keys %{ { @$out } } ;
	my $save = &{ $private{filter} }( \@values ) ;

	while ( @nvp ) {
		$out = &{ $private{matching} }( $self, @{ pop @nvp } ) ;
		@values = keys %{ { @$out } } ;
		$save = &{ $private{filter} }( $save, \@values ) ;
		}

	unless ( wantarray ) {
		my %ashash = map { $_ => $_ } @$save ;
		return bless [ $self, %ashash ], $package ;
		}

	return @$save ;
	}

sub DESTROY {}

sub AUTOLOAD {
	my $self = shift ;
	my $package = ref $self ;

	use vars qw( $AUTOLOAD ) ;
	my $func = $AUTOLOAD ;
	$func =~ s/^${package}::// ;
	return undef unless exists $self->{$func} ;

	my $argct = scalar @_ ;
	my $out = &{ $private{matching} }( $self, $func, @_ ) ;
	return $out if defined $out && ! ref $out ;

	$out ||= [] ;
	my $asarray = bless [ $self, @$out ], $package ;
	return $asarray unless wantarray ;
	return $argct? $asarray->recordID: @$out ;
	}

sub newobject {
	my $package = shift ;
	my $error = shift ;
	my $errortext = pop ;

	return carp( $errortext ) && undef if $_[-1] ;
	return $package->SQLObject( @_, {} ) ;
	}

sub END {
	undef @autodestroy ;
	}


# Preloaded methods go here.

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

NoSQL::PL2SQL::Simple - Implementation of NoSQL::PL2SQL

=head1 SYNOPSIS

  BEGIN {
	package MyArbitraryClass ;
	use base qw( NoSQL::PL2SQL::Simple ) ;
	}

  use CGI ;

  my $collection = new MyArbitraryClass ;

  ## Writing to the database
  $collection->record( CGI->new->Vars ) ;	## Save user input
  $collection->save( CGI->new->Vars ) ;	## save() is an alias

  ## Accessing the database
  @allemails = values %{ { $collection->contactemail } } ;

  $user = $collection->contactemail('jim@tqis.com')->record ;
  @neighbors = $collection->contactaddress('Ann Arbor')->records ;
  @classmates = $collection->query(
		contactcity => 'Ann Arbor',
		graduationyear => '1989',
		)->records ;

=head1 DESCRIPTION

NoSQL::PL2SQL is a low level abstract interface designed to be swapped into existing applications.  It's powerful, flexible, and not quite ready to roll out of the box.

NoSQL::PL2SQL::Simple is an intermediate solution more appropriate for building out new applications.  It retains many of PL2SQL's features, particularly universal data schemas that can accomodate heterogeneous, indeterminate data elements. Additionally, NoSQL::PL2SQL::Simple allows data definition capabilities and provides a complete solution for data access.

NoSQL::PL2SQL::Simple's API consists of the following functions:

=head3 Constructor

C<new()> generally takes no arguments and returns an I<instance> of the data definition class.

Classes that implement NoSQL::PL2SQL::Simple are usually referred to as I<data definition classes>.  Except this document recommends creating additional abstract subclasses called I<data source classes>.  Naturally, the properties of the instance returned by C<new()> reflect the data definition.

Perl OO, in its annoying flexibility, allows class overloading.  So as a convenient mapping of definition and data, objects stored in the database have the same class name.  An I<instance> that contains a data definition is distinguished from a data containing I<object>.

It's appropriate for implementations to create methods that will only be applied to objects.  If so, they should probably override the C<new()> constructor (shown in the example under I<Developer's Notes>) to minimize potential confusion.

=head3 Data Access

Most of NoSQL::PL2SQL::Simple's methods are heavily overloaded.  This approach makes the API easier to remember and minimizes namespace collisions.

C<record()>, for example, has five variants:

=over 8

If its argument is an unblessed object, C<record()> is nearly identical to C<NoSQL::PL2SQL::SQLObject()> in that it simultanously blesses the object and writes it to the database.  Unlike C<NoSQL::PL2SQL::SQLObject()>, C<record()> returns a cloned object that must be explicitly re-saved.

If its argument is a blessed object, C<record()> saves the object into the database.

If its argument is an integer, C<record()> returns an object that was previously written into the database.

If its argument is a blessed array, C<record()> is an alias for C<records()>, which returns a set of objects from the database.

If there are two arguments, the first a blessed object and the second unblessed, C<record()> replaces the first object with the second.

=back

NoSQL::PL2SQL::Simple can be used to maintain the profile records of an access control list:

  ## A new user registers by submitting a website form.  $userid contains
  ## an automatic assignment, which should be returned as part of the
  ## confirmation.

  $userid = $collection->record( CGI->new->Vars )->SQLObjectID ;

  ## If the user logs back in using the $userid, his/her record is
  ## recovered as follows:

  $userrecord = $collection->record( $userid ) ;

  ## Suppose the user changes his/her password.  The change is saved as
  ## follows:

  $userrecord->{password} = $encrypted ;
  $collection->record( $userrecord ) ;

  ## If the website contains a profile editting form, then replace the 
  ## entire record with the form data:

  $collection->record( $userrecord, CGI->new->Vars ) ;

C<recordID()> is a used to convert query results to record id's (otherwise known as object id's).

C<SQLObjectID()> returns the object id automatically generated by C<NoSQL::PL2SQL>.

C<keyValues()> establishes many-to-one and many-to-many relationships among objects, and has three variants:

=over 8

To add a relationships, pass one or more values as arguments to the C<keyValues()> method.

To list relationships, pass no arguments to the C<keyValues()> method.

To clear all relationships, chain the c<keyValues()> method as follows: C<< $o->keyValues()->clear() >>.

=back

See the B<MANY-TO-ONE RELATIONSHIPS> section below.

C<save()> is an alias for C<record()>.  Each of the pair of statements below are equivalent:

  $collection->record( CGI->new->Vars ) ;
  $collection->save( CGI->new->Vars ) ;

  $collection->record( $userrecord ) ;
  $userrecord->save() ;

  $collection->record( $userrecord, CGI->new->Vars ) ;
  $userrecord->save( CGI->new->Vars ) ;

=head3 Data Queries

C<query()> is used to query the database.  Its arguments are name-value-pairs (NVP's).  Multiple NVP's are combined using SQL B<AND> logic.  C<query()> normally returns an array of record id's.  In scalar context, its output can be chained to other methods as illustrated:

  ## In the above example, the user recovers his/her userid by querying
  ## an email address:

  my $cgivars = CGI->new->Vars

  my @userids = $collection->query( 
			contactemail => $cgivars->{contactemail},
			) ;
  error( "Unknown email address" ) if @userids == 0 ;
  error( "Duplicate email address" ) if @userids > 1 ;
  my $userid = $userids[0] ;

  ## Chain recordID to return a scalar instead of an array
  $userid = $collection->query( 
			contactemail => $cgivars->{contactemail} 
			)->recordID ;

  ## Allow the user to login using an email address
  @userids = $collection->query( 
			contactemail => $cgivars->{contactemail},
			password => $encrypted,
			) ;
  error( "Login failed" ) unless @userids == 1 ;
  my $user = $collection->record( $userids[0] ) ;
  my $greeting = sprintf( "Hello %s", $user->{contactnamefirst} ) ;

NoSQL::PL2SQL::Simple uses Perl's magical C<AUTLOAD()> to make simple queries more concise.  The premise of object oriented programming is that complexity is encapsulated inside a black box; and simplicity is exposed at the interface.  The premise of NoSQL::PL2SQL::Simple is that the code below is easy to read and maintain:

  ## The output of query can be chained to return a set of objects
  my @users = $collection->query( 
		contactemail => $cgivars->{contactemail},
		)->records ;

  ## This statement can be rewritten using an AUTOLOAD alias
  @users = $collection->contactemail( $cgivars->{contactemail} )->records ;
  error( "Unknown email address" ) if @users == 0 ;
  error( "Duplicate email address" ) if @users > 1 ;
  my $response = sprintf "Hello %s,\nYour userid is: %d",
		$users[0]->{contactnamefirst}, $users[0]->SQLObjectID ;

  ## Without error checking, this code can be further simplified
  ## In this case, record() aliases records() for readability.
  $user = $collection->contactemail( $cgivars->{contactemail} )->record ;
  $response = sprintf "Hello %s,\nYour userid is: %d",
		$user->{contactnamefirst}, $user->SQLObjectID ;

In this example, the C<contactemail()> method is not explicitly defined.  The magical C<AUTOLOAD()> essentially calls C<query()> using the method name as the first argument.

C<AUTOLOAD()> aliases and C<query()> are slightly different.  The C<AUTOLOAD()> methods are overloaded to take zero or one argument(s).  C<query()> accepts a hash, or even numbered array, as arguments.

If called without arguments, the C<AUTOLOAD()> methods return a hash of NVP's that correspond to the matching objects, keyed on object id.  Each NVP value represents the object element's value.

  ## Allow users to log in using their name, selected from a combo box
  ## 'username' must be part of the data definition
  ## Note:  From a security standpoint, this is a poor practice

  %users = $collection->username ;
  
  ## Build a combo box using the %users NVP
  ## <select name="userid">
  ##  <option value="$userskey">$usersvalue</option>
  ## </select>
  
If called without arguments, C<query()> returns all the stored objects matching the data definition class.  This invocation is used with the C<reindex()> method as shown below.

=head3 Class Definition

A set of NoSQL::PL2SQL::Simple's methods are specifically used as part of the class definition, explained below.  

=over 8

C<addTextIndex()>

C<addNumberIndex()>

C<addDateIndex()>

=back

C<reindex()> updates the database to reflect data definition changes.  It requires a data element argument.  See the B<INDEXES> section below.

=head1 DEFINING CLASSES

Object oriented programming techniques improve code reuse and manage complexity.  For example, an object's behavior is determined by its class definition and its individual properties.  OO designers usually consider tradeoffs between class attributes and object properties.  Subclassing is a technique of assigning attributes to a group of objects so that identical properties don't need to be repetitively defined at instantiation.  Instead, properties are defined as attributes of a subclass instantiated by objects who share those properties.

NoSQL::PL2SQL::Simple uses this approach to separate data definitions from its internal complexity.  Subclassing also ensures that every object is associated with a data definition, which can be accessed as a class instantiation.

=head3 Data Definition Subclass

NoSQL::PL2SQL::Simple is simple even for users who have never defined a class in Perl.  Only the following code is required to define a data definition subclass.  The BEGIN clause is good practice, but not necessary.

  ## This code defines a data definition class named 
  ## 'TQIS::HighSchoolFriends'
  ## The BEGIN clause is not strictly necessary

  BEGIN {
	package TQIS::HighSchoolFriends ;
	use base qw( NoSQL::PL2SQL::Simple ) ;
	}

The actual data definition is built using the data definition methods described above.  This approach should be easier for users who have never written a class definition.  Using a Perl terminal, these statements are simpler than using command line SQL.  Otherwise, the code below could be written into a one-time throw-away script.  Or, using an appropriate Perl interpreter, the data definitions can be easily managed with a web-based tool.

  ## Create another Data Definition Class for this example
  BEGIN {
	package TQIS::HighSchools ;
	use base qw( NoSQL::PL2SQL::Simple )
	}

  ## TQIS::HighSchools contains only a list of schools.  This example
  ## illustrates how to normalize data using NoSQL::PL2SQL::Simple
  TQIS::HighSchools->new->addTextIndex( qw( highschool ) ) ;

  ## Create an instance of the TQIS::HighSchoolFriends data definition
  my $friends = new TQIS::HighSchoolFriends ;

  ## Use the data definition methods to perform the actual data 
  ## definition
  $friends->addTextIndex( qw( yeargraduated lastname ) ) ;

  ## A strictly normalized RDB would use a separate table to reflect
  ## this relationship.  I find this simpler approach of using
  ## internal references more durable: This spouse value will be the
  ## record id of another HighSchoolFriend object.
  $friends->addNumberIndex( qw( spouse ) ) ;
  
  ## Normalizing means these highschool data values will be record
  ## ids from the other data definition class.
  $friends->addNumberIndex( highschool ) ;
  
  ## The data definition is reflected in the properties of the
  ## class instantiation
  print join "\n", %$friends ;

  ## __END__
  ## Throwaway setup script ends here

  ## This remaining code is considered runtime.  For example, this
  ## code would be run to create a web form to add new (or modify)
  ## HighSchoolFriends objects
  %schools = TQIS::HighSchools->new->highschool ;

  ## Populate a web form dropdown with the %schools set, eg:
  ## <select name="highschool">
  ##   <option value="210">Pioneer High School</option>
  ## </select>

  ## Runtime code showing queries using this data definition

  ## This statement chains the constructor with an implicit query
  ## and converts the result to a scalar.
  my $phs = TQIS::HighSchools->new->highschool(
		'Pioneer High School')->recordID ;

  ## A more robust solution is to select the school from a list
  ## Naturally, this works better using a mechanism such as 
  ## a web form combo box
  my %schools = TQIS::HighSchools->new->highschool ;
  my %inverse = reverse %schools ;
  $phs = $inverse{'Pioneer High School'} ;

  ## Incidentally, all these statements will blow up.  Don't try them.
  # $phs = TQIS::HighSchools->new->highschool->{'Pioneer High School'} ;
  # $phs = TQIS::HighSchools->new->{highschool}->{'Pioneer High School'} ;
  # $phs = TQIS::HighSchools->new->highschool('Pioneer High School') ;

  ## The actual query statements
  my @pioneerfriends = $friends->highschool( $phs )->records ;

  or

  my @classmates = $friends->query( 
		highschool => $phs,
		yeargraduated => '1989',
		)->records ;

The data definition only needs to define object properties that will be queried.  See the B<INDEXES> section below.

The definition may also include an I<archive> element.  (Object elements named archive will be ignored.)  The I<archive> data definition prevents object data from being deleted.  Instead, when a record is replaced, the original record is assigned a new object id; and the replacement is inserted into the database with the value of the existing object id. 

  ## Create a new data definition subclass
  BEGIN {
	package TQIS::GPRC::Members ;
	use base ( NoSQL::PL2SQL::Simple ) ;
	}

  ## Create an instantiation
  my $members = new TQIS::GPRC::Members ;

  ## Create a new member from webform data 
  my $memberid = $members->record( CGI->new->Vars )->SQLObjectID ;

  ## An application may email the $memberid as a confirmation.
  ## Later on, the $memberid may be used to replace the member
  ## data with something else
  $members->record( $memberid )->save( CGI->new->Vars ) ;

  ## The new data is returned by default
  my $member = $members->record( $memberid ) ;

  ## Code to return the original data
  my @archive = $members->archive( $memberid )->records ;
  $member = $archive[-1] ;	## returned set is chronological

=head3 Data Source Subclass

Unfortunately, none of the examples presented so far will actually run.  Like any database interface, NoSQL::PL2SQL::Simple needs an actual database.  Advanced users should create an abstract Data Source subclass, described below.  Other users will need to define data sources and pass them to the constructors of each data definition.

NoSQL::PL2SQL::Simple uses its own version of DBI, NoSQL::PL2SQL::DBI.
NoSQL::PL2SQL::DBI contains the methods to generate the specific SQL.  NoSQL::PL2SQL::DBI subclasses are used to handle discrepencies in the SQL syntax.  

NoSQL::PL2SQL is based on a single table which can be shared across multiple implementations.  As in the NoSQL::PL2SQL documentation examples, it's commonly called $dsn.

NoSQL::PL2SQL includes implementations for SQLite and MySQL.  These examples use SQLite, which comes preinstalled.

  ## define a data source
  use NoSQL::PL2SQL::DBI::SQLite ;
  my $dsn = new NoSQL::PL2SQL::DBI::SQLite $tablename = 'objectdata' ;
  $dsn->connect( 'dbi:SQLite:dbname=:memory:', '', '') ;

  ## NoSQL::PL2SQL::Simple queries actually require a second DSN
  my $index = $dsn->table('querydata') ;

  ## run once and only once
  NoSQL::PL2SQL::Simple->loadschema( $dsn, $index ) ;

  ## The DSN objects are passed to the constructor
  my $members = new TQIS::GPRC::Members $dsn, $index ;

  ## The DSN objects can also be shared among all the implementations
  ## we've used in our examples

  my $collection = new MyArbitraryClass $dsn, $index ;
  my $friends = new TQIS::HighSchoolFriends $dsn, $index ;

In order to use the last two statements, we would need to redefine the DSN objects every time we instantiate another data definition subclass.  The resulting code is much less concise than the examples.

The solution is to define an abstract I<data source> subclass.  The code is simple enough to cut and paste from the example below.  But in order to work, this class must be an appropriately named file that can be found using C<@INC>.  For newer users, this might be a good opportunity to learn more about Perl OO programming.

  package TQIS::PL2SQL::DSN
  use base qw( NoSQL::PL2SQL::Simple ) ;	## Do not change this line

  use NoSQL::PL2SQL::DBI::SQLite ;

  my @dsn = () ;				## Do not change this line

  ## data source subclasses override this dsn() method
  sub dsn {
	return @dsn if @dsn ;			## Do not change this line

	push @dsn, new NoSQL::PL2SQL::DBI::SQLite 'objectdata' ;
	$dsn[0]->connect( 'dbi:SQLite:dbname=:memory:', '', '') ;

	push @dsn, $dsn[0]->table('querydata') ;
	return @dsn ;				## Do not change this line
	}

  1 ;

Once complete, the data definition subclasses are very easy to declare:

  BEGIN {
	package MyArbitraryClass ;
	use base qw( TQIS::PL2SQL::DSN ) ;

	package TQIS::HighSchools ;
	use base qw( TQIS::PL2SQL::DSN ) ;

	package TQIS::HighSchoolFriends ;
	use base qw( TQIS::PL2SQL::DSN ) ;

	package TQIS::GPRC::Members ;
	use base qw( TQIS::PL2SQL::DSN ) ;
	}

  ## Run once and only once
  TQIS::PL2SQL::DSN->loadschema() ;

  ## Each of these statements should now succeed
  my $o = new MyArbitraryClass ;
  my $schools = new TQIS::HighSchools ;
  my $friends = new TQIS::HighSchoolFriends ;
  my $members = new TQIS::GPRC::Members ;

=head1 INDEXES

I<Should this section be named INDICES?>

Typically, RDB data is indexed directly on the data set.  NoSQL::PL2SQL::Simple indexes a second table that contains references to the first table.  The biggest drawback to this approach is that records must be explicitly added and deleted from this second table.  Happily, this additional complexity is automatically handled whenever records are added or modified.  Provided the class definition is completed before object data is inserted, this approach should not affect anyone using NoSQL::PL2SQL::Simple.

However, a little housekeeping is required to modify an existing data set:

  my $friends = new TQIS::HighSchoolFriends ;
  $friends->addTextIndex('contactemail') ;

  ## This solution is inefficient, but comprehensive
  map { $friends->record( $_ )->save } $friends->query ;

  ## Alternatively, use this sequence, which must be 
  ## repeated for each new element definition
  map { $friends->record( $_ )->reindex('contactemail') } 
			$friends->query ;

In order to implement NoSQL::PL2SQL's support for indeterminate objects, the class data definition may be completely independent of the object data.  This approach requires explicit mapping, but can be useful:

  my $members = new TQIS::GPRC::Members ;
  $members->addTextIndex('byemail') ;
  
  ## add a new member
  $members->record( CGI->new->Vars, 
		workemail => 'byemail', homeemail => 'byemail' ) ;

  ## This approach creates two records in the index table (three if 
  ## there's an explicit byemail form field), and improves the
  ## data access capabilities as follows:

  ## Both these statements work
  my $jim = $members->byemail('jim@work.tqis.com')->record ;
  $jim = $members->byemail('jim@home.tqis.com')->record ;

  ## this approach must be implemented consistently
  $jim->save( CGI->new->Vars, workemail => 'byemail', homeemail => 'byemail' ) ;

  ## reindexing also allows mapping:
  map { $members->record( $_ )->reindex( workemail => 'byemail' ) }
		$members->query ;
  map { $members->record( $_ )->reindex( homeemail => 'byemail' ) }
		$members->query ;

=head1 MANY-TO-ONE RELATIONSHIPS

Most of the time, database records can be accessed using a one-to-many
relationship.  For example, a calendar application would primarily consist of I<event objects> whose elements include a single date, say February 14.  Many events can share that date, so when I<February 14> is queried, all the matching events can be displayed.

NoSQL::PL2SQL::Simple handles all of these relationships automatically.  If the object's date is changed to February 17, the correspondence from February 14 is automatically broken and all future queries will return expected results.

This example, which assumes that events only correspond to a single date, represents a one-to-many relationship.  Eg, one date corresponds to many events.  In a more complicated data model, events can correspond to many dates, if the event occurs repeatedly or spans more than one day.  This more complicated model is called many-to-many: multiple objects per key and multiple keys per object.

NoSQL::PL2SQL::Simple automates the multiple objects per key functionality; but handling multiple keys per object requires explicit management using the C<keyValues()> method.  In a web-based calendar, the defined properties might be the record owner and location (zipcode).  If event entries always correspond to a single date, then the date will also be a defined property.  NoSQL::PL2SQL::Simple automatically manages queries for defined properties.

If the event can correspond to many dates, the web interface becomes more complicated.  This codes demonstrates several different scenarios:

  my $events = new mycalendar::event ;		## $events is an instance
  print $events->{eventdate}, "\n" ;		## prints: I<datekey>
  my $event = $events->record( CGI->new->Vars ) ;	## $event is an object

  ## One scenario is that the interface returns a set of dates
  ## such as a string containing many separated dates.
  my @dates = split /,/, $event->{datelist} ;
  $event->keyValues( eventdate => @dates ) ;	## add the set of dates

  ## In this scenario, when the record is edited, simply replace the
  ## set of dates:
  $event->keyValues('eventdate')->clear() ;	## clear the existing set
  $event->keyValues( eventdate => split /,/, CGI->new->Vars->{datelist} ) ;

  ## Another scenario is an interface worksheet that adds dates one at a time
  $event->keyValues( eventdate => CGI->new->Vars->{nextdate} ) ;

  ## The named property I<nextdate> must be distinct from the named
  ## property I<eventdate> to prevent automatic key management

  ## Repeat this process everytime a new date is added.  The interface should
  ## prevent a duplicate date selection
  $event->keyValues( eventdate => CGI->new->Vars->{nextdate} ) ;

  ## The interface should allow users to delete date selections
  my $delete = CGI->new->Vars->{deletedate} ;
  my @replace = grep $_ ne $delete, $event->keyValues('eventdate') ;
  $event->keyValues('eventdate')->clear() ;
  $event->keyValues( eventdate => @replace ) ;

=head1 DEVELOPERS NOTES

If you're planning to write an implementation of NoSQL::PL2SQL, take a look at I<samples/pl2sql/comments.pl> in the distribution package.  NoSQL::PL2SQL::Simple is intended to be simple only from the point of view of its interface.

If you're interested in writing OO modules, this module is unusually deliberate and worth noting.

First, the module contains several private methods: C<update()>, C<recno()>, C<filter()>, C<index()>, C<matching()>, and C<indexmap()>.  These methods may disappear in future versions, or require changes where they're called.  So these methods are out of bounds.  Also, since NoSQL::PL2SQL::Simple is intended for subclassing, the number of base methods should be deliberately small to minimize namespace conflicts.  The source code illustrates how to defined these private methods.

Second, I normally would not write a C<new()> constructor- most implementations will want to override the constructor.  But in the interest of a simple API, I prefer constructors with no arguments.  So the C<new()> method is used to generate an I<instance>, which in turn uses C<record()> to create I<objects>.  This approach may be insufficiently robust.

Perl OO generally consists of blessed data structures.  Once blessed, a data structure is associated with specific class methods that can make assumptions about that data.  NoSQL::PL2SQL::Simple is a bit more complicated.  But advanced users may want their data definition classes to include this functionality and define additional class methods.  They should also rename the constructor.

These users should be familiar enough with Perl OO to implement an abstract data source subclass.  One is defined above and used in this example:

  package TQIS::MyAdvancedClass ;
  use base qw( TQIS::PL2SQL::DSN ) ;

  ## Rename the constructor to dataDefinition()
  sub dataDefinition {
	my $package = shift ;
	return bless NoSQL::PL2SQL::Simple::new( $package, @_ ),
			$package ;
	}

  ## Create a default constructor that returns an object instead 
  ## of an instance.  Replace the generic code with something useful.
  sub new {
	my $package = shift ;
	return bless {}, $package ;
	}

  ## Create methods intended for objects, not instances.  This one
  ## prints out a mailing label from a contact record.
  sub mailingLabel {
	my $self = shift ;
	return join '', $self->{name}, "\n",
			$self->{address}, "\n",
			$self->{city}, ' ', 
			$self->{state}, '  ',
			$self->{zipcode}, "\n" ;
	}

  ## __END__

  ## Here's how the class might be used:

  use TQIS::MyAdvancedClass ;

  my $contacts = TQIS::MyAdvancedClass->dataDefinition ;
  print $contacts->email('jim@tqis.com')->record->mailingLabel ;

Third, NoSQL::PL2SQL::Simple uses private properties to avoid potentially overwriting user data.

Using Data::Dumper on both an unblessed structure and its corresponding blessed object might indicate that the two are equivalent.  In fact, the blessed object has additional private properties.  For example, the object id property can only be accessed using the C<SQLObjectID()> method.  Private properties in Perl OO can share a name assigned to a public property without conflict.  

=head2 EXPORT 

None by default.

=head1 SEE ALSO

=over 8

=item NoSQL::PL2SQL

=item NoSQL::PL2SQL::DBI

=item http://pl2sql.tqis.com/

=back

=head1 AUTHOR

Jim Schueler, E<lt>jim@tqis.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2012 Jim Schueler

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.9 or,
at your option, any later version of Perl 5 you may have available.


=cut
