use strict;
use warnings;
use utf8;

use Test::More;
use Test::Exception;
use TM2::TS::Test;

use Data::Dumper;
$Data::Dumper::Indent = 1;

sub _chomp {
    my $s = shift;
    chomp $s;
    return $s;
}

my $warn = shift @ARGV;
unless ($warn) {
    close STDERR;
    open (STDERR, ">/dev/null");
    select (STDERR); $| = 1;
}

use constant DONE => 0;

use TM2;
use Log::Log4perl::Level;
$TM2::log->level($warn ? $DEBUG : $ERROR); # one of DEBUG, INFO, WARN, ERROR, FATAL

use TM2::TempleScript;
use TM2::TempleScript::Parser;

$TM2::TempleScript::Parser::UR_PATH = $warn
    ? '../templescript/ontologies/'
    : '/usr/share/templescript/ontologies/';

unshift  @TM2::TempleScript::Parser::TS_PATHS, './ontologies/';

# sub _parse {
#     my $ref_stm = shift;
#     my $t = shift;

#     use TM2::Materialized::TempleScript;
#     my $tm = TM2::Materialized::TempleScript->new (baseuri => 'tm:')
#         ->extend ('TM2::ObjectAble')
#         ->establish_storage ('*/*' => {})
#         ->extend ('TM2::Executable')
#         ->extend ('TM2::ImplementAble')
#         ->extend( 'TM2::StackAble' )
#             ->stack_under( $ref_stm )
#         ;

#     $tm->deserialize ($t);
#     return $tm;
# }

sub _mk_ctx {
    my $stm = shift;
    return [ { '$_'  => $stm, '$__' => $stm } ];
}


#-- TESTS ----------------------------------------------------------

use TM2::TempleScript::Parser;
$TM2::TempleScript::Parser::UR_PATH = '../templescript/ontologies/';

unshift  @TM2::TempleScript::Parser::TS_PATHS, './ontologies/';
# my $core = TM2::Materialized::TempleScript->new (
#     file    => $TM2::TempleScript::Parser::UR_PATH . 'core.ts',
#     baseuri => 'ts:')
#         ->extend ('TM2::ObjectAble')
#         ->extend ('TM2::ImplementAble')
#     ->sync_in;
# my $env = TM2::Materialized::TempleScript->new (
#     file    => $TM2::TempleScript::Parser::UR_PATH . 'env.ts',
#     baseuri => 'ts:')
#         ->extend ('TM2::ObjectAble')
#         ->extend ('TM2::ImplementAble')
#     ->sync_in;

# my $sco = TM2::TempleScript::Stacked->new (orig => $core, id => 'ts:core');
# my $sen = TM2::TempleScript::Stacked->new (orig => $env,  id => 'ts:environment', upstream => $sco);


if (DONE) {
    my $AGENDA = q{ontology: };

#     my $stm = $sen;
#     my $tm = _parse (\$stm, q{

# %include file:lxd.ts

# });

#     my $ctx = _mk_ctx ($stm);

    use IO::Async::Loop;
    my $loop = IO::Async::Loop->new;

#    $ctx = [ @$ctx, { '$loop' => $loop } ];


    use TM2::Virtual::lxd;
    my $tm  = TM2::Virtual::lxd->new( url => 'https://192.168.3.50:8443', loop => $loop);
    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm));

    my $tss;
#--
    $tss = TM2::TempleScript::return ($ctx, q{ ( lxd:object >> instances )  });

    ok( (grep { $_ eq 'lxd:Certificate' } map { $_->[0] } @$tss), $AGENDA.'basic concepts found');
}

if (1||DONE) {
    my $AGENDA = q{project lifecycle: };

    use IO::Async::Loop;
    my $loop = IO::Async::Loop->new;
    if (1) {
	my $tm = TM2::Virtual::lxd->new( url => 'https://192.168.3.50:8443', loop => $loop);

	use TM2::TempleScript::Stacked;
	my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm));

	my $tss = TM2::TempleScript::return ($ctx, q{ ( 
        """{
            "lxd:Project:testp" => {
	      "config" => {
		"features.images"   => "false",
		"features.profiles" => "false"
	      },
	      "description" => "TempleScript test suite",
	      "name" => "testp",
           }
	}""" ^^ lang:perl ! ++> <~[ lxd/project ]~ $_ )  });
#warn Dumper $tss;
	is_singleton( $tss, undef, $AGENDA.'one result');
	isa_ok( $tss->[0]->[0]->{'lxd:Project:testp'}->{config}, 'HASH', $AGENDA.'perlified copy of project spec');
#warn Dumper $tm;
	ok( $tm->{mid2iid}->{'lxd:Project'},       $AGENDA.'white box test');
	ok( $tm->{mid2iid}->{'lxd:Project:testp'}, $AGENDA.'white box test');
	ok( $tm->tids('lxd:Project:testp'),        $AGENDA.'project topic exists');
	ok( $tm->{mid2iid}->{'lxd:Project:testp'}, $AGENDA.'white box test, still');

	is_deeply([ $tm->tids('lxd:Project:testq') ], [ undef ],        $AGENDA.'project topic not exists');

#warn Dumper $tm->{mid2iid}->{'lxd:Project:testp'};
	ok( $tm->is_a ('lxd:Project:testp', 'lxd:Project'), $AGENDA.'lxd:Project class settled' );
#warn Dumper $tm->{mid2iid}->{'lxd:Project:testp'};
#--
	throws_ok {
	    TM2::TempleScript::return ($ctx, q{ (
        """{
            "lxd:Project:testp" => {
	      "config" => {
		"features.images"   => "false",
		"features.profiles" => "false"
	      },
	      "description" => "TempleScript test suite",
	      "name" => "testp",
           }

	}""" ^^ lang:perl ! ++> <~[ lxd/project ]~ $_ )  });
	} qr/exist/, $AGENDA.'project already exists';
#--
	$tss = TM2::TempleScript::return ($ctx, q{ ( 
        """{
           "lxd:Project:testp" => undef
	}""" ^^ lang:perl ! --> <~[ lxd/project ]~ $_ )  });
#warn Dumper $tss;
	is_singleton( $tss, undef, $AGENDA.'one result');
	is( $tss->[0]->[0]->{'lxd:Project:testp'}, undef, $AGENDA.'perlified result of DELETE');
#warn Dumper $tm->tids('lxd:Project:testp');
#exit;
	is_deeply([ $tm->tids('lxd:Project:testp') ], [ undef ],        $AGENDA.'project topic not exists anymore');
#-- multiple create
	$tss = TM2::TempleScript::return ($ctx, q{ ( 
        """{
            "lxd:Project:testp" => {
	      "config" => {
		"features.images"   => "false",
		"features.profiles" => "false"
	      },
	      "description" => "TempleScript test suite",
	      "name" => "testp",
           },
            "lxd:Project:testq" => {
	      "config" => {
		"features.images"   => "false",
		"features.profiles" => "false"
	      },
	      "description" => "TempleScript test suite",
	      "name" => "testq",
           },
	}""" ^^ lang:perl ! ++> <~[ lxd/project ]~ $_ )  });
	is_singleton( $tss, undef, $AGENDA.'one result');
	isa_ok( $tss->[0]->[0]->{'lxd:Project:testp'}->{config}, 'HASH', $AGENDA.'perlified copy of project spec');
	isa_ok( $tss->[0]->[0]->{'lxd:Project:testq'}->{config}, 'HASH', $AGENDA.'perlified copy of project spec');

	is( (scalar
	     grep { m{testp|testq} }
	     @{ $tm->_lxd->projects->get }), 2, $AGENDA.'2 projects created');
#--
	$tss = TM2::TempleScript::return ($ctx, q{ ( 
        """{
           "lxd:Project:testp" => undef,
           "lxd:Project:testq" => undef
	}""" ^^ lang:perl ! --> <~[ lxd/project ]~ $_ )  });
	is( (scalar
	     grep { m{testp|testq} }
	     @{ $tm->_lxd->projects->get }), 0, $AGENDA.'2 projects deleted');
	
warn Dumper $tss;
# delete single id, list of ids	
# delete non ex
# delete all
# ||> ==> __>
    }
}
exit;
if (DONE) {
    my $AGENDA = q{container instances: };

    use IO::Async::Loop;
    my $loop = IO::Async::Loop->new;
    if (1) {

	my $tm = TM2::Virtual::lxd->new( url => 'https://192.168.3.50:8443', loop => $loop);

	use TM2::TempleScript::Stacked;
	my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm));

	my $tss = TM2::TempleScript::return ($ctx, q{ ( lxd:Instance >> instances )  });
	ok( eq_set( [ map { $_->[0] } @$tss ], [ qw( lxd:Instance:test1 lxd:Instance:test2 ) ] ), $AGENDA.'instances directly on $lxd map');
    }
    if (1) {
	my $tm = _parse (qq{
%include file:ontologies/lxd.atm

});

	use TM2::TempleScript::Stacked;
	my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm));

	my $lxd = TM2::Virtual::lxd->new( url => 'https://192.168.3.50:8443', loop => $loop);
	$ctx->[0]->{ '$lxd' } = $lxd;

	my $tss = TM2::TempleScript::return ($ctx, q{ -{ $lxd => $_ }-{ ( lxd:Instance >> instances ) }-  });
	ok( eq_set( [ map { $_->[0] } @$tss ], [ qw( lxd:Instance:test1 lxd:Instance:test2 ) ] ), $AGENDA.'instances via junction');
#warn Dumper $tss;
    }
    if (1) {
	my $tm = _parse (qq{
%include file:lxd.ts

});

	use TM2::TempleScript::Stacked;
	my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm));
	$ctx = [ @$ctx, { '$loop' => $loop } ];

	my $tss = TM2::TempleScript::return ($ctx, q{ ( "https://192.168.3.50:8443" ) |->> ts:fusion( lxd:host )  });
	is_singleton( $tss, undef, $AGENDA.'single lxd');
	isa_ok( $tss->[0]->[0], 'TM2::Virtual::lxd');
	is ($tss->[0]->[0]->_lxd->endpoint, 'https://192.168.3.50:8443', $AGENDA.'endpoint');
#--
	$tss = TM2::TempleScript::return ($ctx, q{ -{ "https://192.168.3.50:8443" |->> ts:fusion( lxd:host ) => $_ }-{ ( lxd:Instance >> instances ) }-  });
	ok( eq_set( [ map { $_->[0] } @$tss ], [ qw( lxd:Instance:test1 lxd:Instance:test2 ) ] ), $AGENDA.'instances via junction');
#warn Dumper $tss;
    }
}


if (DONE) {
    my $AGENDA = q{auto configuration for properties: };

    use IO::Async::Loop;
    my $loop = IO::Async::Loop->new;

    my $tm = TM2::Virtual::lxd->new( url => 'https://192.168.3.50:8443', loop => $loop );

    use TM2::TempleScript::Stacked;
    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm));

    if (1) {
	my $tss = TM2::TempleScript::return ($ctx, q{ ( lxd:Certificate >> instances )  });
	my $fp  = $tss->[0]->[0]; $fp =~ s{lxd:Certificate:}{};

	$tss = TM2::TempleScript::return ($ctx, q{ ( lxd:Certificate >> instances >> characteristics lxd:fingerprint )  });
	is_deeply( $tss->[0]->[0], TM2::Literal->new( $fp, TM2::Literal->STRING ), $AGENDA.'property drill fingerprint' );
	$tss = TM2::TempleScript::return ($ctx, q{ ( lxd:Certificate >> instances / lxd:fingerprint )  });
	is_deeply( $tss->[0]->[0], TM2::Literal->new( $fp, TM2::Literal->STRING ), $AGENDA.'property drill fingerprint' );
#--
	$tss = TM2::TempleScript::return ($ctx, q{ ( lxd:Certificate >> instances >> characteristics name )  });
	is_deeply( $tss->[0]->[0], TM2::Literal->new( "127.0.0.1" ), $AGENDA.'property drill name' );
	$tss = TM2::TempleScript::return ($ctx, q{ ( lxd:Certificate >> instances >> characteristics lxd:name )  });
	is_deeply( $tss->[0]->[0], TM2::Literal->new( "127.0.0.1" ), $AGENDA.'property drill name' );
	$tss = TM2::TempleScript::return ($ctx, q{ ( lxd:Certificate >> instances / name )  });
	is_deeply( $tss->[0]->[0], TM2::Literal->new( "127.0.0.1" ), $AGENDA.'property drill name' );
#--
	$tss = TM2::TempleScript::return ($ctx, q{ ( lxd:Certificate >> instances >> characteristics lxd:certificate )  });
	like( $tss->[0]->[0]->[0], qr/BEGIN.+END/s, $AGENDA.'property drill certificate' );
#--
	$tss = TM2::TempleScript::return ($ctx, q{ ( lxd:Certificate >> instances >> characteristics lxd:projects )  });
	ok (! scalar(@$tss), $AGENDA.'no projects');
#warn Dumper $tss; exit;
#--
	throws_ok {
	    TM2::TempleScript::return ($ctx, q{ ( lxd:Certificate >> instances >> characteristics lxd:object )  });
	} qr/cannot identify/, $AGENDA.'object as property';
#warn Dumper $tss;
    }
    if (1) {
	my $tss = TM2::TempleScript::return ($ctx, q{ ( lxd:Image >> instances )  });
	$tss = TM2::TempleScript::return ($ctx, q{ ( lxd:Image >> instances >> characteristics lxd:architecture )  });
warn Dumper $tss;
    }
}

done_testing;

__END__


if (DONE) {
    my $AGENDA = q{timed operations: };

    use File::Temp qw/ tempdir /;
    my $root = tempdir( CLEANUP => 1 );

    my $tm = _parse (qq{

%include file:ontologies/lucy.atm

aaa
! AAA is nice guy

bbb
! BBB is also a nice man

ccc
! CCC is not such a nice person

ddd
! DDD is definitely an unnice person

#--

isa storage @ lang:perl :
   class       : fulltext-query
   ts:software : urn:x-perl:TM2::ObjectAble::StringsViaLucy

fulltext-query
  ~ urn:x-mime:text/query
lucy:root        : $root
lucy:auto-create : 1
lucy:unique-tid  : 1 # at every STORE we look whether that tid is there and is to be deleted
lucy:expiry      : "3.1 sec"

}.q{
§ isa ts:converter
    ts:mimetype @ ts:input  : "text/plain"
    ts:mimetype @ ts:output : "text/query"
return """
   return TM2::Literal->new ($_[0]->[0], 'urn:x-mime:text/query');
""" ^^ lang:perl !

});

    use TM2::TempleScript::Stacked;
    my $ctx = _mk_ctx (TM2::TempleScript::Stacked->new (orig => $tm));

    my $tss;
#--
    use TM2::ObjectAble::StringsViaLucy;
    $tss = TM2::TempleScript::return ($ctx, q{ ( aaa, bbb ) | zigzag | ( $0 / name ==> <~[text/query]~ $0 ) });  # fill index
    is ((scalar @$tss), 2, $AGENDA.'indexing person1 names');
    sleep 2;
    $tss = TM2::TempleScript::return ($ctx, q{ ( ccc ) | ( $0 / name ==> <~[text/query]~ $0 ) });  # fill index
    sleep 1;
    $tss = TM2::TempleScript::return ($ctx, q{ ( ddd ) | ( $0 / name ==> <~[text/query]~ $0 ) });  # fill index
#--
    sleep 1;
    $tss = TM2::TempleScript::return ($ctx, q{ ( aaa, bbb, ccc, ddd ) | zigzag | ( $0  ~[text/query]~> ) });  # forward lookup
    ok (eq_set ([ map { $_->[0]->[0] } @$tss ],
		[
		    'CCC is not such a nice person',
		    'DDD is definitely an unnice person'
		]), $AGENDA.'expired result');
#warn Dumper $tss
}

done_testing;

__END__


    is_deeply ($tss, [ [ 'tm:aaa' ], [ 'tm:bbb' ] ], 'reverse');
    $tss = TM2::TempleScript::return ($ctx, q{ ( "hell-o-world" <~[xxx/yyy]~ ) });
    is_deeply ($tss, [ [ 'tm:aaa' ], [ 'tm:bbb' ] ], 'reverse');
#--
    throws_ok {
        TM2::TempleScript::return ($ctx, q{ ( "hell-o-world" << zoomer aaa/bbb) });
    } qr/cannot find/, 'unknown implicit conversion';
#--
    $tss= TM2::TempleScript::return ($ctx, q{ ( "hell-o-world" << zoomer xxx/zzz) });
    ok (! @$tss, 'unwilling to reverse xxx/yyy');
#--
    $tss = TM2::TempleScript::return ($ctx, q{ ( "hell-o-world" << zoomer ) });
    ok (! @$tss, 'unwilling to reverse */*');
    $tss = TM2::TempleScript::return ($ctx, q{ ( "hell-o-world" <~~ ) });
    ok (! @$tss, 'unwilling to reverse */*');
#    warn Dumper $tss;
    use TM2::ObjectAble::StringViaPlucene;

    use File::Temp qw/ tempdir /;
    my $root = tempdir( CLEANUP => 1 );

    my %hash1;
    tie %hash1, 'TM2::ObjectAble::StringViaPlucene', root => $root;
    $tm->storage ('xxx/*' => \%hash1);

    my %hash2;
    tie %hash2, 'InvertAbleHash';
    $tm->storage ('xxx/yyy' => \%hash2);

    $tm->objectify ('xxx/zzz',       'tm:bbb', new TM2::Literal ('hell-o-world')); # goes into xxx/*
    $tm->objectify ('xxx/yyy',       'tm:aaa', new TM2::Literal ('hell-o-world')); # goes into xxx/yyy/, reversible
    $tm->objectify ('zzz/xxx',       'tm:aaa', new TM2::Literal ('hell-o-world')); # goes into */*

if (DONE) {
    my $AGENDA = q{object atomification: };
    {
	package PERSON;
	use Data::Dumper;
	use Moose;
	has 'age'  => (is => 'rw', 'isa' => 'Num');
	has 'name' => (is => 'rw', 'isa' => 'Str');
	has 'url'  => (is => 'rw', 'isa' => 'Str');
	around 'BUILDARGS' => sub {
	    my $orig = shift;
	    my $class = shift;
#warn "params ".Dumper \@_;
	    return $class->$orig (name => $_[0]->[0], age => $_[1]->[0], url => $_[2]->[0]);
	};
	1;
    }
    my $tm = _parse (q{

person isa class
 ~ urn:x-whatever:person

isa implementation @ lang:perl :
  class       : person
  ts:software : urn:x-perl:PERSON

aaa isa person
! AAA
age : 23
homepage : http://aaa.com/

bbb isa person
! BBB
age : 24
homepage : http://bbb.com/

});

    my $ctx = [ { '$_' => $tm, '$__' => $tm } ];
#--
    my $tss;

    if (DONE) {
	my $SUBAGENDA = $AGENDA.q{without package: };
	$tss = TM2::TempleScript::return ($ctx, q{
               ( person >> instances ) .. ( ./name, ./age, ./homepage ) 
    });
#	warn Dumper $tss; exit;

	ok ((scalar @$tss) == 2, $SUBAGENDA.'implicit atomification: exactly two tuples');
	map { is ( ref ($_), 'TM2::Literal', $SUBAGENDA.'implicit atomification: all literals '.$_->[0]) } @{ $tss->[0] };
	map { is ( ref ($_), 'TM2::Literal', $SUBAGENDA.'implicit atomification: all literals '.$_->[0]) } @{ $tss->[1] };
    }

    if (DONE) {
	my $SUBAGENDA = $AGENDA.q{with Perl package: };
	$tss = TM2::TempleScript::return ($ctx, q{
               ( person >> instances) .. ( ./name, ./age, ./homepage ) .. ( atomify person )
        });

	ok ((scalar @$tss) == 2, $SUBAGENDA.'explicit atomification: exactly two tuples');
	map { ok (scalar @$_ == 1, $SUBAGENDA.'explicit atomification: each tuple has length 1')  } @$tss;
	ok (eq_set ([ map { $_->[0]->name } @$tss],
		    [ 'AAA', 'BBB' ]), $SUBAGENDA.'explicit atomification: object names');
	ok (eq_set ([ map {$_->[0]->age } @$tss],
		    [ 23, 24 ]), $SUBAGENDA.'explicit atomification: object ages');
	ok (eq_set ([ map {$_->[0]->url } @$tss],
		    [ 'http://aaa.com/', 'http://bbb.com/' ]), $SUBAGENDA.'explicit atomification: object ages');
    }
    if (DONE) {
	my $SUBAGENDA = $AGENDA.q{with Perl package (via indicator): };
	$tss = TM2::TempleScript::return ($ctx, q{
`               ( person >> instances) .. ( ./name, ./age, ./homepage ) .. ( atomify urn:x-whatever:person )
        });

	ok ((scalar @$tss) == 2, $SUBAGENDA.'explicit atomification: exactly two tuples');
	map { ok (scalar @$_ == 1, $SUBAGENDA.'explicit atomification: each tuple has length 1')  } @$tss;
	ok (eq_set ([ map { $_->[0]->name } @$tss],
		    [ 'AAA', 'BBB' ]), $SUBAGENDA.'explicit atomification: object names');
	ok (eq_set ([ map {$_->[0]->age } @$tss],
		    [ 23, 24 ]), $SUBAGENDA.'explicit atomification: object ages');
	ok (eq_set ([ map {$_->[0]->url } @$tss],
		    [ 'http://aaa.com/', 'http://bbb.com/' ]), $SUBAGENDA.'explicit atomification: object ages');
    }
}

if (DONE) {
    my $AGENDA = q{crunch, as Perl list: };
    my $tm = _parse (q{

aaa isa person
! AAA
age : 23
homepage : http://aaa.com/

bbb isa person
! BBB
age : 24
homepage : http://bbb.com/

});

    my $ctx = [ { '$_' => $tm, '$__' => $tm } ];
    my $tss = TM2::TempleScript::return ($ctx, q{
             ( ( person >> instances) .. ( ./name, ./age, ./homepage ) crunch )
    });
#	warn Dumper $tss; exit;
    ok ((scalar @$tss) == 2, $AGENDA.'exactly two tuples');
    map { ok (scalar @$_ == 1, $AGENDA.'each tuple has length 1')  } @$tss;
    ok (eq_set ([ map {$_->[0]->[0]->[0] } @$tss],
		[ 'AAA', 'BBB' ]), $AGENDA.'object values');
}

done_testing;

__END__
