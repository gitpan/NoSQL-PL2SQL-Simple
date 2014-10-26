# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl NoSQL-PL2SQL-Simple.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 55;
BEGIN { use_ok('XML::Parser::Nodes') };
BEGIN { use_ok('NoSQL::PL2SQL::DBI::SQLite') };
BEGIN { use_ok('NoSQL::PL2SQL::DBI') };
BEGIN { use_ok('NoSQL::PL2SQL') };
BEGIN { use_ok('NoSQL::PL2SQL::Simple') };

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

sub testpath {
	my @path = split m|/|, ":$0" ;
	pop @path ;
	$path[0] =~ s/^:// ;
	return join '/', @path, @_ ;
	}

sub XML::Parser::Nodes::asobject {
	my $node = shift ;
	my %data = map { $_->[0], $_->[1]->gettext } $node->childnodes ;
	return \%data ;
	}

do {
	package TQIS::DataSource ;
	use base qw( NoSQL::PL2SQL::Simple ) ;

	my @dsn = () ;

	sub dsn {
		return @dsn if @dsn ;
		push @dsn, NoSQL::PL2SQL::DBI::SQLite->new('objectdata') ;
		push @dsn, $dsn[0]->table('querydata') ;
		$dsn[1]->connect( 'dbi:SQLite:dbname=:memory:', '', '') ;
		return @dsn ;
		}

	package TQIS::Employees ;
	use base qw( TQIS::DataSource ) ;

	package TQIS::Contacts ;
	use base qw( TQIS::DataSource ) ;
	} ;

## Test data source subclass TQIS::Datasource

my @dsn = TQIS::Employees->dsn ;
is( scalar @dsn => 2 ) ;
is( $dsn[0]->table => 'objectdata' ) ;

@dsn = TQIS::Employees->dsn ;
TQIS::DataSource->loadschema ;

## Test loadschema().  Ensure it's called independently on separate 
## data sources.  Throws an error, otherwise.

my $testdsn = new NoSQL::PL2SQL::DBI::SQLite 'objecttest' ;
$testdsn->connect( 'dbi:SQLite:dbname=:memory:', '', '') ;
NoSQL::PL2SQL::Simple->loadschema( $testdsn, 'querytest' ) ;

## I wanted to test this at a reasonable scale.  So around 17K records
## are loaded from a MySQL test data set.

my $testdatafn = testpath( 'employees.xml' ) ;
exit unless -f $testdatafn ;

my $nodes = new XML::Parser::Nodes $testdatafn ;
my @staff = map { $_->asobject } 
		$nodes->childnode('testdata')->childnode('employees') ;
my @cent = splice @staff, -100, 100 ;

## Data definitions

my $employees = new TQIS::Employees ;
$employees->addDateIndex( qw( birthday startdate ) ) ;
$employees->addTextIndex( qw( gender ) ) ;

## re-instantiating the data definition should not be necessary
# undef $employees ;
# my $employees = new TQIS::Employees ;

## Load a bunch of records

map { $employees->record( $_ ) } @staff ;

## re-instantiating the data definition should not be necessary
# my $employees = new TQIS::Employees ;

## How many records, a reasonably large group
my @query = $employees->query ;
is( scalar @query => 16899 ) ;

## The hash output of AUTOLOAD() aliases is described as a normalizing
## feature

my %july = $employees->startdate ;
is( keys %july => 16899 ) ;

my %julyct = () ;
map { $julyct{ $july{$_} } ||= [] ; push @{ $julyct{ $july{$_} } }, $_ }
		keys %july ;
is( scalar @{ $julyct{'1988-07-14'} } => 7 ) ;

## Should get the same result with an argument

my @july = $employees->startdate( '1988-07-14' ) ;
is( scalar @july => 7 ) ;

## Test a multi-dimensional search using query()

@july = $employees->query( startdate => '1988-07-14', gender => 'M' ) ;
is( scalar @july, 2 ) ;

@july = $employees->query( startdate => '1988-07-14', gender => 'F' ) ;
is( scalar @july, 5 ) ;

## Test re-indexing

while (1) {
#	last ;
	$employees->addTextIndex( qw( lastname ) ) ;
	map { $employees->record( $_ )->reindex('lastname') } 
			$employees->query ;
	last ;
	}

## Query the new data set

my @family = $employees->lastname('Schueler') ;
is ( scalar @family => 15 ) ;

## Re-test multi-dimensional search

@family = $employees->query( lastname => 'Schueler', gender => 'M' 
		)->records ;
is( scalar @family, 4 ) ;

## Look for a particular record among the results
## (Should've named one of my kids Xiaoqiang Schueler)

my @xiaoqiang = grep $_->{birthday} eq '1963-04-26', @family ;
is( scalar @xiaoqiang => 1 ) ;

@family = $employees->query( lastname => 'Schueler', gender => 'F' ) ;
is( scalar @family => 11 ) ;

## A multidimensional search that fails one vector...

@xiaoqiang = $employees->query( 
		lastname => 'Schueler', 
		gender => 'F', 
		birthday => '1963-04-26' ) ; 
is( scalar @xiaoqiang => 0 ) ;

## Should show up in one query or the other...

@xiaoqiang = $employees->query( 
		lastname => 'Schueler', 
		gender => 'M', 
		birthday => '1963-04-26' ) ; 
is( scalar @xiaoqiang => 1 ) ;

## Before adding a new record, ensure it hasn't been loaded yet

my @gronowski = $employees->query( lastname => 'Gronowski',
		birthday => '1962-10-07',
		) ;
is( scalar @gronowski => 0 ) ;

## Load Mr. Gronowski

my $id = $employees->record( shift @cent )->SQLObjectID ;

## Record count should reflect his change

@query = $employees->query ;
is( scalar @query => 16900 ) ;

## Query that failed earlier should now succeed.

@gronowski = $employees->query( lastname => 'Gronowski',
		birthday => '1962-10-07',
		) ;
is( scalar @gronowski => 1 ) ;

## Test SQLObjectID()- same result as default query output

is( $id => $gronowski[0] ) ;

## Mr. Gronowski will now change his name

my $record = $employees->record( $id ) ;
$record->{lastname} = 'Krassner' ;
$record->save ;

## Query that succeeded earlier should now fail

@gronowski = $employees->query( lastname => 'Gronowski',
		birthday => '1962-10-07',
		) ;
is( scalar @gronowski => 0 ) ;

## Query that failed earlier should succeed with the name change

@gronowski = $employees->query( lastname => 'Krassner',
		birthday => '1962-10-07',
		) ;
is( scalar @gronowski => 1 ) ;

## Test using AUTOLOAD alias.  Also ensures that lastname data definition
## is persistent

my @krassner = $employees->lastname('Krassner') ;
is( scalar @krassner => 1 ) ;

## Ensure that record used to be Mr. Gronowski's

my $krassner = $employees->lastname('Krassner')->record ;
is( $krassner->SQLObjectID => $id ) ;

## Pull out one of the Schueler records, and replace with another

my $core = shift @cent ;
my $family = $family[4] ;
$employees->record( $family )->save( $core ) ;

## After two replacements, the record count should remain unchanged

@query = $employees->query ;
is( scalar @query => 16900 ) ;

## One fewer Schueler than before

@family = $employees->query( lastname => 'Schueler', gender => 'F' ) ;
is( scalar @family => 10 ) ;

## Record now contains replacment data

is( $employees->record( $family )->{firstname}, 'Marsja' ) ;

## Search criteria based on replacement data

@query = $employees->query( lastname => 'Besancenot',
			birthday => '1960-10-21' )->records ;

## Search results should uniquely identify the former Schueler record

is( scalar @query => 1 ) ;
is( $query[0]->SQLObjectID => $family ) ;

## Test the archive feature.  Modify the data definition

TQIS::Employees->new->addNumberIndex('archive') ;
$employees = new TQIS::Employees ;

## Record is visible to searches

my @schueler = $employees->query( lastname => 'Schueler',
		birthday => '1957-07-23' ) ;
is( scalar @schueler => 1 ) ;

## Swap out another Schueler record

$id = $employees->record( $schueler[0] )->{id} ;
$employees->record( $schueler[0] )->save( shift @cent ) ;

## Numbers are dwindling...

@family = $employees->query( lastname => 'Schueler', gender => 'F' ) ;
is( scalar @family => 9 ) ;

## Three revisions, record count remains unchanged

@query = $employees->query ;
is( scalar @query => 16900 ) ;

## Old record, new data

is( $employees->record( $schueler[0] )->{id} => 26902 ) ;

## Archived record found?

my @ok = $employees->archive( $schueler[0] )->records ;
is( scalar @ok => 1 ) ;

## Results match original data?

is( $ok[0]->{id} => $id ) ;

## Record is hidden from searches

@query = $employees->query( lastname => 'Schueler',
		birthday => '1957-07-23' ) ;
is( scalar @query => 0 ) ;

@ok = $employees->archive( $schueler[0] )->recordID ;

## Record ID changed

ok( $ok[0] != $schueler[0] ) ;

## Archived record is invisible

@query = $employees->query ;
@ct = grep $_ == $ok[0], @query ;
is( scalar @ct => 0 ) ;

## Create another data definition

$#cent = 89 ;
my $contacts = new TQIS::Contacts ;
$contacts->addDateIndex( qw( birthday ) ) ;
$contacts->addTextIndex( qw( lastname ) ) ;

## load a few records with the new definition

map { $contacts->record( $_ ) } @cent ;

## Ensure the defintions remain distinct

@query = $employees->query ;
is( scalar @query => 16900 ) ;

@query = $contacts->query ;
is( scalar @query => 90 ) ;

## Shows up as a new Contacts definition, not an Employee definition

@ok = $employees->query( lastname => 'Tischendorf',
		birthday => '1955-04-29' )->records ;
is( @ok => 0 ) ;

@ok = $contacts->query( lastname => 'Tischendorf',
		birthday => '1955-04-29' )->records ;
is( @ok => 1 ) ;

## Added keyvalues() method

TQIS::Employees->new->addTextIndex('dependents') ;
$employees = new TQIS::Employees ;

@schueler = $employees->lastname('Schueler')->records ;
$id = $schueler[0]->SQLObjectID ;

@ok = $schueler[0]->keyValues('dependents') ;
warn "\n TQIS ", $ok[0] ;
is( @ok => 0 ) ;

$schueler[0]->keyValues( dependents => qw( Eva Hans Clara ) ) ;
@ok = $schueler[0]->keyValues('dependents') ;
is( @ok => 3 ) ;

@ok = $employees->dependents('Clara')->recordID ;
is( @ok => 1 ) ;
is( $ok[0] => $id ) ;

@ok = $employees->dependents('Hans')->recordID ;
is( @ok => 1 ) ;
is( $ok[0] => $id ) ;

@ok = $employees->dependents('Eva')->recordID ;
is( @ok => 1 ) ;
is( $ok[0] => $id ) ;

$schueler[0]->keyValues('dependents')->clear ;
@ok = $schueler[0]->keyValues('dependents') ;
is( @ok => 0 ) ;

@ok = $employees->dependents('Eva')->recordID ;
is( @ok => 0 ) ;


1
