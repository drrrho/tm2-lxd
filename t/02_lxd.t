use strict;
use warnings;
use Data::Dumper;

use Test::More;
use Test::Exception;

use constant DONE   => 0;

my $warn = shift @ARGV;
unless ($warn) {
    close STDERR;
    open (STDERR, ">/dev/null");
    select (STDERR); $| = 1;
}

use TM2;
use Log::Log4perl::Level;
$TM2::log->level ($DEBUG) if $warn;

use TM2::TempleScript;
use TM2::TempleScript::Parser;

$TM2::TempleScript::Parser::UR_PATH = $warn
    ? '../templescript/ontologies/'
    : '/usr/share/templescript/ontologies/';

unshift  @TM2::TempleScript::Parser::TS_PATHS, './ontologies/';


use_ok ('TM2::Virtual::lxd');

use IO::Async::Loop;
my $loop = IO::Async::Loop->new;

if (1||DONE) {
    my $AGENDA = q{constructor: };
    throws_ok {
	TM2::Virtual::lxd->new ();
    } qr{no url}i, 'no url provided';

    my $tm = TM2::Virtual::lxd->new( url => 'https://192.168.3.50:8443', loop => $loop);
    isa_ok( $tm->_lxd, 'Net::Async::WebService::lxd'); #, $AGENDA.'host handle');
}

if (1||DONE) {
    my $AGENDA = q{ontology: };
    my $tm = TM2::Virtual::lxd->new( url => 'https://192.168.3.50:8443', loop => $loop );

    ok( (scalar $tm->instances( 'lxd:object' ) ) > 10, $AGENDA.'found lxd:object instances');

    my @tos = $tm->toplets;
    ok( (map { $_->[0] =~ /lxd:Image/ }       @tos), $AGENDA.'key concepts'); # class
    ok( (map { $_->[0] =~ /lxd:fingerprint/ } @tos), $AGENDA.'key concepts'); # properties
    ok( (map { $_->[0] =~ /occurrence/ }      @tos), $AGENDA.'key concepts'); # original
#--
    @tos = $tm->toplets('lxd:Image', 'lxd:fingerprint');
    ok( eq_set([ map { $_->[0] } @tos ], [ qw(lxd:Image lxd:fingerprint ) ]), $AGENDA.'selected toplets');
#--
    @tos = $tm->toplets(\ '+all -infrastructure');
    ok( ! (grep { $_ eq 'name' } map {$_->[0]} @tos), $AGENDA.'no infrastructure');
}

if (DONE) {
    my $AGENDA = q{containers: };
    my $tm = TM2::Virtual::lxd->new( url => 'https://192.168.3.50:8443', loop => $loop );

    ok( eq_set([ $tm->instances ('lxd:Instance') ],
	       [ qw(lxd:Instance:test1 lxd:Instance:test2 ) ]), $AGENDA.'instances' );

    ok( eq_set([ $tm->types( 'lxd:Instance:test1' ) ], [ qw(lxd:Instance) ]), $AGENDA.'instance type');
}

done_testing;

__END__


if (DONE) {
    my $tm = TM2::Virtual::CPAN::viaSqlite->new (url => 'file:t/cpandb.sql');

    is ($tm->tids ('distribution'), 'cpan:distribution', 'native vocabulary');
    is ($tm->tids ('module'),       'cpan:module',       'native vocabulary');

    is ($tm->tids (\ 'urn:x-cpan:TM'),      'distribution:TM', 'detected distribution tid');
    is ($tm->tids (\ 'urn:x-cpan:TMXXXX'),  undef,             'detected non-distribution tid');
    is ($tm->tids (\ 'urn:x-perl:TM'),      'module:TM',       'detected module tid');
    is ($tm->tids (\ 'urn:x-perl:TMXXX'),   undef,             'detected non-module tid');
    is ($tm->tids (\ 'urn:x-cpanid:DRRHO'), 'author:DRRHO',    'detected author tid');
    is ($tm->tids (\ 'urn:x-cpanid:DRRHOXXX'), undef,          'detected non-author tid');

#    warn Dumper [    $tm->tids (\ 'urn:x-cpan:TM') ];

    throws_ok {
	$tm->toplets ('aaa');
    } qr/cannot/, 'no parameter for toplets';

    my @ts = $tm->toplets;

    ok ((grep { $_->[0] eq 'author:DRRHO' }    @ts), 'author toplets found');
    ok ((grep { $_->[0] eq 'module:TM' }       @ts), 'module toplets found');
    ok ((grep { $_->[0] eq 'distribution:TM' } @ts), 'distribution toplets found');
}

unlink ('/tmp/xxx', '/tmp/__db*');


unlink ('/tmp/xxx', '/tmp/__db*');

if (DONE) { # distributions
    my $tm = TM2::Virtual::CPAN::viaSqlite->new (url => 'file:t/cpandb.sql');

#    throws_ok {
#	$tm->filter_class ([], 'cpan:distribution');
#    } qr{unwilling}, 'no memory enumeration of everything';

    use TM2::DBI;
    my $h = TM2::DBI->handle( "dbi:SQLite:dbname=t/cpandb.sql" );

    my $sth = $h->prepare ("SELECT COUNT(*) FROM dists");
    $sth->execute;
    my ($N) = $LIMIT || $sth->fetchrow_array;
    {
	my $out;
	use TM2::TS::BerkeleyDB;
	tie @$out, 'TM2::TS::BerkeleyDB', '/tmp/xxx';
	$tm->filter_class ($out, 'cpan:distribution');
	is ((scalar @$out), $N, "as many distributions in all-list ($N)");
	untie @$out;
    }

    my $R = 2;
    for my $r (1..$R) {
	my $out;
	use TM2::TS::BerkeleyDB;
	tie @$out, 'TM2::TS::BerkeleyDB', '/tmp/xxx';
#warn Dumper $out;
#warn Dumper $out->[5]; exit;
	my $sth = $h->prepare ("SELECT dist_name FROM dists WHERE dist_id = ? ");
	for (1..10) {  # random samples
	    $sth->execute (int (rand ($N)));
	    my ($name) = $sth->fetchrow_array;
#warn "$N -> $name";
	    next unless defined $name;
	    ok ((grep { $_->[0]->[TM2->PLAYERS]->[1] eq "distribution:$name" } @$out), "found $name in all-list (run $r/$R)");
	}
	untie @$out;
    }
    {
	my $out;
	use TM2::TS::BerkeleyDB;
	tie @$out, 'TM2::TS::BerkeleyDB', '/tmp/xxx';

	my $db = tied (@$out);
	untie @$out;
	$db->_DESTROY;
    }
}

if (DONE) { # authors
    my $tm = TM2::Virtual::CPAN::viaSqlite->new (url => 'file:t/cpandb.sql');

    use TM2::DBI;
    my $h = TM2::DBI->handle( "dbi:SQLite:dbname=t/cpandb.sql" );

    my $sth = $h->prepare ("SELECT COUNT(*) FROM auths");
    $sth->execute;
    my ($N) = $sth->fetchrow_array;
    {
	my $out;
	use TM2::TS::BerkeleyDB;
	tie @$out, 'TM2::TS::BerkeleyDB', '/tmp/xxx';
	@$out = ();
	$tm->filter_class ($out, 'cpan:author');
	is ((scalar @$out), $N, "as many authors in all-list ($N)");
	untie @$out;
    }

    my $R = 2;
    for my $r (1..$R) {
	my $out;
	use TM2::TS::BerkeleyDB;
	tie @$out, 'TM2::TS::BerkeleyDB', '/tmp/xxx';

	my $sth = $h->prepare ("SELECT cpanid FROM auths WHERE auth_id = ?");
	for (1..10) {  # random samples
	    $sth->execute (int (rand ($N)));
	    my ($name) = $sth->fetchrow_array;
	    ok ((grep { $_->[0]->[TM2->PLAYERS]->[1] eq "author:$name" } @$out), "found $name in all-list (run $r/$R)");
	}
	untie @$out;
    }
    {
	my $out;
	use TM2::TS::BerkeleyDB;
	tie @$out, 'TM2::TS::BerkeleyDB', '/tmp/xxx';

	my $db = tied (@$out);
	untie @$out;
	$db->_DESTROY;
    }
}

if (DONE) { # modules
    my $tm = TM2::Virtual::CPAN::viaSqlite->new (url => 'file:t/cpandb.sql');

#    throws_ok {
#	$tm->filter_class ([], 'cpan:module');
#    } qr{unwilling}, 'no memory enumeration of everything';

    use TM2::DBI;
    my $h = TM2::DBI->handle( "dbi:SQLite:dbname=t/cpandb.sql" );

    my $sth = $h->prepare ("SELECT COUNT(*) FROM mods");
    $sth->execute;
    my ($N) = $LIMIT || $sth->fetchrow_array;
    {
	my $out;
	use TM2::TS::BerkeleyDB;
	tie @$out, 'TM2::TS::BerkeleyDB', 'xxx';
	@$out = ();
	$tm->filter_class ($out, 'cpan:module');
	is ((scalar @$out), $N, "as many modules in all-list ($N)");
	untie @$out;
    }

    my $R = 2;
    for my $r (1..$R) {
	my $out;
	use TM2::TS::BerkeleyDB;
	tie @$out, 'TM2::TS::BerkeleyDB', 'xxx';

	my $sth = $h->prepare ("SELECT mod_name FROM mods WHERE mod_id = ?");
	for (1..5) {  # random samples
	    $sth->execute (int (rand ($N)));
	    my ($name) = $sth->fetchrow_array;
	    next unless defined $name;
	    ok ((grep { $_->[0]->[TM2->PLAYERS]->[1] eq "module:$name" } @$out), "found $name in all-list (run $r/$R)");
	}
	untie @$out;
    }
    {
	my $out;
	use TM2::TS::BerkeleyDB;
	tie @$out, 'TM2::TS::BerkeleyDB', 'xxx';

	my $db = tied (@$out);
	untie @$out;
	$db->_DESTROY;
    }
}

done_testing;

END {
    unlink ('/tmp/xxx', '/tmp/__db*');
}

__END__

exit;
if (DONE) { # typing
    my $tm = TM2::Virtual::CPAN::viaSqlite->new (url => 'file:t/cpandb.sql', baseuri => 'cpan:');

    throws_ok {
	$tm->match_forall;
    } qr{unwilling}, 'no enumeration of everything';

    {
	my @as1 = $tm->match_forall (class => 'cpan:distribution', type => 'isa');
	my @as2 = $tm->retrieve ( map { $_->[TM2->LID] } @as1 );
	ok (eq_array (\@as1, \@as2), 'all instances of distributions are recorded');
    }
    {
	my @as1 = $tm->match_forall (class => 'cpan:module', type => 'isa');
	warn Dumper \@as1; exit;
	my @as2 = $tm->retrieve ( map { $_->[TM2->LID] } @as1 );
	ok (eq_array (\@as1, \@as2), 'all instances of modules are recorded');
    }


    exit;
    ok (eq_set ([ $tm->types ('cpan:dist:TM') ],
		[ 'cpan:distribution' ]), 'classified distribution');


    warn Dumper [    $tm->instances ('cpan:distribution') ];

#    ok (eq_set ([ $tm->types ('cpan:xxx') ],
#		[ 'cpan:distribution' ]), 'classified distribution (manual)');


}

done_testing;

__END__

use constant STRESS => 0;

if (DONE) {
    throws_ok {
         new TM2::Materialized::CPAN (url     => 'http:t/minicpan/', 
				      baseuri => 'tm:',
				      limit   => 10,
	     );
    } qr/protocol/, "invalid file name";
}

if (DONE) {
    my $tm = new TM2::Materialized::CPAN (url     => 'file:t/minicpan/', 
					  baseuri => 'tm:',
					  limit   => 10,
	);
    $tm->sync_in;

    is (10, (scalar $tm->instancesT ($tm->tids ('distribution'))),              'found limit distributions');
    is (10, (scalar $tm->instancesT ($tm->tids ('software'))),                  'found software (ontology)');

    is ($tm->tids ('ACH'), 'tm:ACH',                                            'found ACH distribution');

    ok ((grep { $_ eq 'tm:ACH' } $tm->instancesT ($tm->tids ('distribution'))), 'found ACH distribution');

# no addons
    is (0, (scalar $tm->match_forall (char => 1, type => 'tm:filename',    topic => 'tm:ACH')), 'no filename addon');
    is (0, (scalar $tm->match_forall (char => 1, type => 'tm:module',      topic => 'tm:ACH')), 'no module addon');
}

if (DONE) {
    my $tm = new TM2::Materialized::CPAN (url      => 'file:t/minicpan/',
					  baseuri  => 'tm:',
					  skiplist => [ qr/^Acme/i ],
					  limit    => 100,
	);
    $tm->sync_in;
    ok (! (grep { $_ =~ /tm:Acme/i } $tm->instances ($tm->tids ('distribution'))), 'found no Acme distribution');
}

if (DONE) { #-- addons
    my $tm = new TM2::Materialized::CPAN (url     => 'file:t/minicpan/',
                                         baseuri => 'tm:',
                                         limit   => 10,
					 addons  => [ qw(filename modules) ]
	);
    $tm->sync_in;
    ok (eq_set ([ map { @{ $_->[TM2->INDICATORS] } } $tm->toplets ('tm:ACH') ],
		[ 'urn:x-cpan:ACH' ]), 'permalink is subject indicator');

    my ($value, $type) =  map  { @$_ }
                          map  { $_->[TM2->PLAYERS]->[1] }
                          $tm->match_forall (char => 1, type => 'tm:filename', topic => 'tm:ACH');
    like ($value,    qr{authors/id/C/CP/CPKOIS/ACH/ACH}, 'filename path');
    like ($type,     qr{URI},                            'filename type');

    my @modules        =  map  { $_->[TM2->PLAYERS]->[1]->[0] }
                          $tm->match_forall (char => 1, type => 'tm:module', topic => 'tm:AAC-Pvoice');
    is ((scalar grep { $_ =~ /urn:x-perl:AAC::/ } @modules), 7, 'all modules have urls');

    ok (eq_set ([ map { $_ =~ /urn:x-perl:(.+)$/ && $1 }  @modules ],
		[ qw(AAC::Pvoice::Row AAC::Pvoice::Dialog AAC::Pvoice::Bitmap AAC::Pvoice::Panel AAC::Pvoice AAC::Pvoice::EditableRow AAC::Pvoice::Input ) ]),
	'modules of AAC-Pvoice');

    # ($value, $type)   =  map  { @$_ }
    #                      map  { $_->[TM2->PLAYERS]->[1] }
    #                      $tm->match_forall (char => 1, type => 'tm:permalink',   topic => 'tm:ACH');
    # like ($value,     qr{http://search.+ACH/$},           'permalink path');
    # like ($type,      qr{URI},                            'permalink type');

}

if (DONE) {
    my $tm = new TM2::Materialized::CPAN (url     => 'file:t/minicpan/',
                                         baseuri => 'tm:',
                                         limit   => 10,
					 addons  => [ qw(authors) ]
	);
    $tm->sync_in;
    
    ok ($tm->instances ($tm->tids ('person')), 'found persons');

    my ($cpanid) =       map  { TM2::get_players   ($tm, $_, 'tm:author') }
                         $tm->match_forall (type => 'tm:is-author-of', irole => 'tm:work', iplayer => 'tm:ACH');
    is ($cpanid, 'tm:cpanid-CPKOIS', 'found author');

    my ($value, $type) = map  { @$_ }
                         map  { $_->[TM2->PLAYERS]->[1] }
                         $tm->match_forall (char => 1, type => 'name',   topic => $cpanid);
    is   ($value,     'Christopher Kois',           'author fullname');
    like ($type,      qr{string},                   'author type');

    ($value, $type)   =  map  { @$_ }
                         map  { $_->[TM2->PLAYERS]->[1] }
                         $tm->match_forall (char => 1, type => 'tm:email',   topic => $cpanid);
    is   ($value,     'mailto:cpkois@cpan.org',        'author mail');
    like ($type,      qr{URI},                         'author type');
}


__END__



authors

    if (DONE) {

tagging => [ viaChapters, 
	     viaNamespace,
	     viaCPANforum, 


]
}


http://cpanforum.com/tags/


ignore:
Simple
Easy

    Split:
DecisionTree
FuzzyInference

chapters ignore
Misc
Bundles
Documentation
Perl6
