package lxd::projects;

use strict;
use warnings;
use Data::Dumper;

use TM2;

require Tie::Hash;
our @ISA = qw(Tie::StdHash);

sub TIEHASH  {
    my $class = shift;
    my %options = @_;
    $TM2::log->logdie ("no lxd handled provided") unless $options{'lxd'};

    return bless \%options, $class;
}


sub STORE {
    my $self  = shift;
    my $key   = shift;   # only the tid
    my $value = shift;   # a complete HASH with MIME => value entries

#warn "STORE projects $key => ". (Dumper [ caller ]) ; # . Dumper $value;
#warn "\\_ $key ".Dumper $value;

#    my $v = eval $value->[0];
#warn Dumper $v;

    use Data::Structure::Util qw(unbless);
    unbless($value); # we do not want to upset JSON

    my $res = $self->{lxd}->create_project( body => $value )->get;
    $TM2::log->logdie( "lxd REST problem: $res" ) unless $res eq 'Success';
}

sub DELETE {
    my $self  = shift;
    my $key   = shift;   # only the tid

warn "DELETE projects $key => "; #. (Dumper [ caller ]) ; # . Dumper $value;
    if ($key =~ /lxd:Project:(\w+)/) {
	my $res = $self->{lxd}->delete_project( name => $1 )->get;
#warn Dumper $res;
    } else {
	$TM2::log->warn( "requesting to delete '$key' as LXD project, ignored." );
    }
}

1;

package TM2::Virtual::lxd;

use strict;
use warnings;
use Data::Dumper;

use Moose;
extends 'TM2';                   # it *is* a topic map, after all
with 'TM2::ResourceAble';
with 'TM2::ObjectAble';
with 'TM2::Virtualized';

has 'mime' => (
    is  => 'ro',
    isa => 'Str',
    default => 'templescript/map' );

has '_lxd' => (
    is  => 'ro',
    isa => 'Net::Async::WebService::lxd',
    );

has 'loop' => (
    is  => 'ro',
    isa => 'IO::Async::Loop' );

around BUILDARGS => sub {
    my $orig  = shift;
    my $class = shift;

    my %options;
    if (ref ($_[0]) eq 'TM2::Literal') { # url as literal
	$options{url} = shift->[0];
    } elsif ($_[0] =~ /(https|file:)/) { # url as string
	$options{url} = shift;
    }

    %options = (%options, @_);

    $TM2::log->logdie ("no URL provided")       unless $options{url};

    use Net::Async::WebService::lxd;
    my $lxd = Net::Async::WebService::lxd->new( loop               => $options{loop},
						endpoint           => $options{url},
						client_cert_file   => "t/client.crt",
						client_key_file    => "t/client.key",
						server_fingerprint => 'sha1$92:DD:63:F8:99:C4:5F:82:59:52:82:A9:09:C8:57:F0:67:56:B0:1B',
	                                      );
    return $class->$orig (%options,
			  baseuri => 'lxd:',
			  _lxd    => $lxd,
	                  );
};



my $ONTOLOGY = 'lxd.atm';
my @PATHS = qw(./ontologies/ ../tm2-lxd/ontologies/ /usr/share/templescript/ontologies/ );

my $ontology; # will be computed with first object construction
my $axes;     # will be computed at first object construction


sub BUILD {
    my $elf = shift;

    sub _detect_ontology {
        foreach  (map { $_ . $ONTOLOGY } @PATHS) {
            return $_ if -e $_;
        }
        $TM2::log->logdie ("cannot detect '$ONTOLOGY' in paths ".join (", ", @PATHS));
    }
    sub _sanitize {
	my $t = shift;
	my $s = shift;
#warn ">>>$s<<<";
	$s =~ s/\n/ /sg;
	$s =~ s/$t\s+represents\s+(an?|the)?\s*//;
#warn "===$s===";
#warn "                                                ";
	return $s;
    }
    sub _derive_ontology {
	my $defs = $Net::Async::WebService::lxd::rest_api->{definitions};

	my $onto;
#	$onto .= "%include file:core.ts\n\n";
	$onto .= "%include file:lxd.ts\n\n";
	$onto .= join "\n",
	         map { "$_->[0] isa lxd:object
! $_->[1]
" }
                 map { [ "lxd:$_", _sanitize( $_, $defs->{$_}->{title} //  $defs->{$_}->{description} ) ] }
	         grep { $_ !~ /(Put|Get|Post)$/ }
                 keys %$defs
	         ;
	my %props;
	map { $props{$_}++ }
	    map { keys %{ $_->{properties} } }
	    map { $defs->{$_} }
	    grep { $_ !~ /(Put|Get|Post)$/ }
	    keys %$defs;
	$onto .= "\n";
	$onto .= join "\n",
	         map { "lxd:$_ isa lxd:property
! $_
" }
                 keys %props,
	         qw(os description) # those NOT mentioned explicitly in the spec
	         ;
	return $onto;
    }
    sub _derive_axes {
	my $rest_api = shift;
	my @classes = ( sort # to be refined
	      grep { $_ !~ /StorageVolumePostTarget|ClusterMemberConfigKey|ServerStorageDriverInfo/ }
	      grep { $_ !~ /^Resources/i }
	      grep { $_ !~ /(warning|operation|event|cluster|server)/i }
	      grep { $_ !~ /(add|join)Token$/i }
	      grep { $_ !~ /(put|post)$/i }
	      keys %{ $rest_api->{definitions} } );

#warn Dumper [ sort keys %{ $Net::Async::WebService::lxd::META } ]; exit;
	my $META = $Net::Async::WebService::lxd::META;
	@classes = grep { defined $META->{ lc($_) || 1 } } @classes; # narraw down to classes where we have known methods # FIXME!!
	my $axes;
	foreach my $class (@classes) {
#warn "class $class";
#warn Dumper [ sort keys %{ $rest_api->{paths} } ];
	    my ($path) = grep { $_ =~ qr|/1.0/${class}s/\{.+\}|i } keys %{ $rest_api->{paths} }
	                   or $TM2::log->logdie("cannot identify key drilling operation for '$class' in REST API");
	    my $key = $path =~ qr|\{(\w+)\}| && $1;
	    foreach my $property (keys %{ $rest_api->{definitions}->{ $class }->{ properties }} ) {
		$axes->{ $class } ->{ $property } = {
		    method   => lc($class),
		    key      => $key,
		    property => $property,
		};
	    }
	}
	return $axes;
    }

#warn ">>>> "._derive_ontology; exit;
    use TM2::Materialized::TempleScript;
    $ontology //= TM2::Materialized::TempleScript->new (inline  => _derive_ontology,
							baseuri => $elf->baseuri)
        ->sync_in;                                                         # really load the ontology
    $elf->melt ($ontology);                                                # glue it to the map
    $elf->{'.storages'} = $ontology->{'.storages'};                        # also move all functionality

    my %projects;
    tie %projects, 'lxd::projects', 'lxd' => $elf->_lxd;
    $elf->establish_storage ('lxd/project' => \%projects);

#warn Dumper $ontology; exit;
    $axes //= _derive_axes( $Net::Async::WebService::lxd::rest_api );

    return $elf;
}


sub last_mod {
    my $self = shift;
    return $self->created;                                                 # we do not want the file mtime, but the time this was "synced" in
}


around 'tids' => sub {
    my $orig = shift;
    my $self = shift;

#warn "tids virtual".Dumper \@_;
    my @tids;
    foreach my $t (@_) {
	if (!ref($t) && $t =~ /^lxd:Project:(\w+)/) {
	    my $this = $1;
	    my @projects = @{ $self->_lxd->projects( )->get };
#warn "pro '$this' ".Dumper \@projects;
#warn "\\_ ".Dumper [ grep { $_ eq "/1.0/projects/$this" } @projects ];
	    if (grep { $_ eq "/1.0/projects/$this" } @projects) {
		push @tids, $t;
	    } else {
		push @tids, undef;
	    }
	} else {
	    push @tids, $self->$orig ($t);
	}
    }
    return wantarray ? @tids : $tids[0];
};


around 'filter_instance' => sub {
    my $orig = shift;
    my $self = shift;
    my $out  = shift;
    my $i    = shift;

#warn "XXXXXXX instance ".Dumper ($self->{mid2iid}->{'lxd:Project:testp'}) . Dumper [ caller ];
    if ($i =~ /^lxd:Instance:/) {
        push @$out, [ _ca ($self,
                           scope   => 'us',
                           type    => 'isa',
                           roles   => [ 'class',        'instance' ],
                           players => [ 'lxd:Instance', $i ]) ] ;

    } elsif ($i =~ /^lxd:Project:/) {
        push @$out, [ _ca ($self,
                           scope   => 'us',
                           type    => 'isa',
                           roles   => [ 'class',        'instance' ],
                           players => [ 'lxd:Project', $i ]) ] ;

    } else {
        $self->$orig ($out, $i);
    }
#warn "YYYYYYYY instance ".Dumper ($self->{mid2iid}->{'lxd:Project:testp'}) . Dumper [ caller ];
    return $out;
};


around 'filter_class' => sub {
    my $orig = shift;
    my $self = shift;
    my $bu   = $self->baseuri;
    my $out  = shift;
    my $c    = shift;

#warn "XXXXXXX class ".Dumper ($self->{mid2iid}->{'lxd:Project:testp'}) . Dumper [ caller ];
    my $lxd  = $self->_lxd;

    my $paths = $Net::Async::WebService::lxd::rest_api->{paths};  # TODO: remove this dependency
    if ($c eq 'lxd:object') { # we have those preloaded
        $self->$orig ($out, $c);

    } elsif ($c =~ /^lxd:(\w+)/) {
	my $operation = lc($1).'s';
	if ($paths->{ "/1.0/$operation" }->{get}) { # there is a method to retrieve a list of these things
	    push @$out, map { [ _ca ($self,
				     scope   => 'us',
				     type    => 'isa',
				     roles   => [ 'class',  'instance' ],
				     players => [ $c,       $_ ]) ] }
	            map { s{/1.0/$operation/(.+)}{$c:$1}; $_ }
	            @{ $lxd->$operation( )->get };

	} else {
	    $TM2::log->logdie( "cannot handle instances of type '$c'" );
	}


    } else {
        $self->$orig ($out, $c);
    }
#warn "YYYYYYYY class ".Dumper ($self->{mid2iid}->{'lxd:Project:testp'}) . Dumper [ caller ];
    return $out;
};

around 'filter_topic_type' => sub {
    my $orig = shift;
    my $self = shift;
    my $bu   = $self->baseuri;
    my $out  = shift;
    my $c    = shift;
    my $t    = shift; # topic id

    my $lxd  = $self->_lxd;

    my $paths = $Net::Async::WebService::lxd::rest_api->{paths};

#warn "lxd::Virtual filter_topic_type $t $c";

    if ($t =~ /^(lxd:(\w+)|(name))/) { # recognized lxd property, or name
	my $property = $2 // $3;
#warn "found property $property";
	if ($c =~ /lxd:(\w+):(.+)/) { # recognized lxd thing
	    my $class    = $1;
	    my $instance = $2;
#warn "found thing of $class";
	    if (my $access = $axes->{ $class }->{ $property }) {
		my $operation = $access->{ method };
#warn "found op $operation ";
		my $key = $access->{ key };
#warn "found key $key for $instance";
		my $res = $lxd->$operation( $key => $instance )->get;  # wait until we got the whole structure back
		my $val = $res->{ $property };
		if (defined $val) { # if there is a defined value for that property
		    if (ref( $val ) eq 'ARRAY') {
			push @$out, map { [ TM2::Literal->new( $_, TM2::Literal->STRING ) ] } @$val;
		    } elsif (ref( $val ) eq 'HASH') {
			push @$out, [ $val ];
		    } else {
			push @$out, [ TM2::Literal->new( $val, TM2::Literal->STRING ) ]; # STRING, because it is essential a JSON string
		    }
		} # otherwise empty result
	    } else {
		$TM2::log->logdie("cannot identify axes for class '$class' property '$property' in REST API");
	    }

	} else {
	    $TM2::log->logdie("did not recognize id as lxd:class:thing structure in '$c'");
	}

    } else {
#warn "looking for t=$t and c=$c in orig map";
        $self->$orig ($out, $c, $t);
    }
    return $out;
};

sub instances {
    my $self = shift;
    my @is;
    $self->filter_class (\@is, @_);
#warn Dumper @is;
    return map { $_->[0]->[TM2->PLAYERS]->[1] } @is;
}

sub types {
    my $self = shift;
    my @ty;
    $self->filter_instance (\@ty, @_);
    return map { $_->[0]->[TM2->PLAYERS]->[1] } @ty;
};


#--

sub _ca { # complete assertion
    my $tm = shift;
    my ($a) = $tm->normalize ( Assertion->new (@_) );
    return $a;
}

sub normalize { # similar to original, but NO internalize
    my $self = shift;
    my ($THING, $US) = ('thing', 'us');

    foreach (@_) {                                             # only now use all the information to complete the assertions
	unless ($_->[TM2->CANON]) {
	    $_->[TM2->KIND]  ||= TM2->ASSOC;
	    $_->[TM2->TYPE]  ||= $THING;
	    $_->[TM2->SCOPE] ||= $US;

	    TM2::canonicalize (undef, $_);

	    $_->[TM2->LID]   ||= TM2::mklabel ($_);
	}
    }
    return @_;
}

1;


__END__

#warn Dumper $axes;
	# my $axes2 = { Certificate => { 
	#     'fingerprint' => {
	# 	method => 'certificate',
	# 	key => 'fingerprint',
	# 	property => 'fingerprint',
	#     },
	#     'name' => {
	# 	method => 'certificate',
	# 	key => 'fingerprint',
	# 	property => 'name',
	#     },
	#     'certificate' => {
	# 	method => 'certificate',
	# 	key => 'fingerprint',
	# 	property => 'certificate',
	#     },
	#     'projects' => {
	# 	method => 'certificate',
	# 	key => 'fingerprint',
	# 	property => 'projects',
	#     },
	# 	     },
	# };
# around 'tids' => sub {
#     my $orig = shift;
#     my $self = shift;
#     my $bu   = $self->baseuri;

#     my @ks;
#     foreach (@_) {
# warn "virtual lxd tids working on $_".Dumper $_;
# 	push @ks, $self->$orig ($_);
#      }
# warn "cpan tids \\ result ".Dumper \@ks;
#     return wantarray ? @ks : $ks[0];
# };

# 	if (ref ($_)) {                                             # we got a subject indicator
# 	    my $si = $$_;
# 	    $si =~ s{http://templescript.org/ns/cpan/distribution/}{urn:x-cpan:};
# 	    $si =~ s{http://templescript.org/ns/cpan/module/}{urn:x-perl:};
# 	    $si =~ s{http://templescript.org/ns/cpan/author/}{urn:x-cpanid:};


# 	} elsif (/^distribution:(.+)$/) {

# 	} else {
# 	}


# around 'consistency' => sub {
#     my $orig = shift;
#     my $elf  = shift;
#     my $bu   = $elf->baseuri;

#     my $lxd = $elf->_lxd;

# warn "YYYYYYYY instances ".Dumper \@_;
#     my @instances;
#     foreach my $t (@_) {
# 	if ($t eq 'lxd:Instance') {
# 	    push @instances, map { s{/1.0/instances/(.+)}{${bu}Instance:$1}; $_ } @{ $lxd->instances( )->get };
# 	} else {
# 	    push @instances, $elf->$orig ( $t );
# 	}
#     }
# #warn "xxxx".Dumper \@instances;
#     return @instances;
# };


# sub internalize {
#     my $self = shift;

# warn "virtual internalize";

#     my @ids; # agenda
#     while (1) {
#     	my ($id, $sid) = (shift, shift);
# 	if (defined $id) {
# 	    push @ids, $id;
# 	} elsif (defined $sid) {
# 	    my $tid = $self->tids ($sid);
# 	    push @ids, $tid;
# 	} else { # both undef
# 	    last;
# 	}
#     }
#     return wantarray ? @ids : $ids[0];
# }

# sub tids {
#     my $self = shift;
#     my $bu   = $self->baseuri;
# warn "virtual lxd tids in $self $bu ".Dumper \@_;

#     my @ks;
#      foreach (@_) {
# 	 warn "tids working on $_".Dumper $_;
# 	 if (ref ($_)) {                                             # we got a subject indicator
# 	     my $si = $$_;
# warn $si;
# 	     push @ks, $si;
# 	 } else {
# #	     push @ks, $self->$orig ($_);
# 	 }
#      }
#  #warn "lxd tids \\ result ".Dumper \@ks;
#      return wantarray ? @ks : $ks[0];
# }


# 	    $si =~ s{http://templescript.org/ns/cpan/distribution/}{urn:x-cpan:};
# 	    $si =~ s{http://templescript.org/ns/cpan/module/}{urn:x-perl:};
# 	    $si =~ s{http://templescript.org/ns/cpan/author/}{urn:x-cpanid:};


# 	} elsif (/^distribution:(.+)$/) {


has 'dbi' => (                # path/file name of the database
    is => 'ro',
    isa => 'Str'
    );


our $DBI_OPTIONS = {
#    'InactiveDestroy' => 1,
#    'AutoInactiveDestroy' => 1,
#    'ReadOnly' => 1
 };


our $LIMIT = ''; # 'LIMIT 2000';


my $ONTOLOGY = 'cpan.ts';

my @PATHS = qw(./ontologies/ ../tm2_cpan/ontologies/ /usr/share/templescript/ontologies/ );

sub BUILD {
    my $self = shift;

    sub _detect_ontology {
        foreach  (map { $_ . $ONTOLOGY } @PATHS) {
            return $_ if -e $_;
        }
        $TM2::log->logdie ("cannot detect '$ONTOLOGY' in paths ".join (", ", @PATHS));
    }

    use TM2::Materialized::TempleScript;
    my $o = TM2::Materialized::TempleScript->new (file    => _detect_ontology,
                                                  baseuri => 'cpan:')
        ->sync_in;                                                         # really load the ontology
    $self->melt ($o);                                                      # glue it to the map
    return $self;
}

# sub DEMOLISH {
# warn "DEMOLISH cpan viasqlite";
# }


sub sync_out {
    $TM2::log->info ("sync out of virtual CPAN ignored");
}

sub internalize {
    my $self = shift;

    my @ids; # agenda
    while (1) {
    	my ($id, $sid) = (shift, shift);
	if (defined $id) {
	    push @ids, $id;
	} elsif (defined $sid) {
	    my $tid = $self->tids ($sid);
	    push @ids, $tid;
	} else { # both undef
	    last;
	}
    }
    return wantarray ? @ids : $ids[0];
}

around 'toplets' => sub {
    my $orig = shift;
    my $self = shift;
    my $bu   = $self->baseuri;

    $TM2::log->logdie ("cannot handle toplets parameters ") if scalar @_;
#warn "virtal cpan toplets CPAN: original $self $orig ";
    my $h = TM2::DBI->handle( $self->dbi );

    my @ts; # AGENDA
    push @ts, $self->$orig ();
    {
        my $sth = $h->prepare( qq{SELECT dist_name FROM dists $LIMIT}) 
            or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
        $sth->execute
            or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
        while (my ($name) = $sth->fetchrow_array) {
            push @ts, [ 'distribution:'.$name, undef, [ 'urn:x-cpan:'.$name ] ];
        }
    }
    {
        my $sth = $h->prepare( q{SELECT cpanid FROM auths}) 
            or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
        $sth->execute
            or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
        while (my ($name) = $sth->fetchrow_array) {
            push @ts, [ 'author:'.$name, undef, [ 'urn:x-cpanid:'.$name ] ];
        }
    }
    {
        my $sth = $h->prepare( qq{SELECT mod_name FROM mods $LIMIT}) 
            or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
        $sth->execute
            or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
        while (my ($name) = $sth->fetchrow_array) {
            push @ts,  [ 'module:'.$name, undef, [ 'urn:x-perl:'.$name ] ];
        }
    }
    return @ts;
};

around 'tids' => sub {
    my $orig = shift;
    my $self = shift;
    my $bu   = $self->baseuri;

#warn "virtal cpan tids CPAN: original $self $orig params ".Dumper \@_;
    my $h = TM2::DBI->handle( $self->dbi );

    my @ks;
    foreach (@_) {
#warn "tids working on $_".Dumper $_;
	if (ref ($_)) {                                             # we got a subject indicator
	    my $si = $$_;
	    $si =~ s{http://templescript.org/ns/cpan/distribution/}{urn:x-cpan:};
	    $si =~ s{http://templescript.org/ns/cpan/module/}{urn:x-perl:};
	    $si =~ s{http://templescript.org/ns/cpan/author/}{urn:x-cpanid:};

	    if ($si =~ /^urn:x-cpan:(.+)$/) {             # that indicating an CPAN distribution
                my $dist = $1;
                $dist =~ s/::/-/g;
		push @ks, _lookup_name ($h, 'dists', $dist)
		               ? 'distribution:'.$dist 
			       : undef;


	    } elsif ($si =~ /^urn:x-perl:(.+)$/) {        # that indicating an CPAN module
#warn "CPAN tid $si";
                my $mod = $1;
                $mod =~ s/::/-/g;
		push @ks, _lookup_name ($h, 'mods', $mod)
		               ? 'module:'.$mod
			       : undef;
#warn "result ks ".Dumper \@ks;

	    } elsif ($si =~ /^urn:x-cpanid:(.+)$/) {      # that indicating a CPAN author
		push @ks, _lookup_name ($h, 'auths', $1)
		               ? 'author:'.$1
			       : undef;

	    } else {
#                warn "   $_ else non of specific".Dumper $self;
		push @ks, $self->$orig ($_);
	    }

	} elsif (/^distribution:(.+)$/) {
	    my $dist = $1;
	    $dist =~ s/::/-/g;
	    push @ks, _lookup_name ($h, 'dists', $dist)
		               ? 'distribution:'.$dist 
			       : undef;

	} elsif (/^module:(.+)$/) {
	    my $mod = $1;
	    $mod =~ s/::/-/g;
	    push @ks, _lookup_name ($h, 'mods', $mod)
		               ? 'module:'.$mod
			       : undef;

	} elsif (/^author:(.+)$/) {
	    push @ks, _lookup_name ($h, 'auths', $1)
		               ? 'author:'.$1
			       : undef;

	} else {
	    push @ks, $self->$orig ($_);
	}
    }
#warn "cpan tids \\ result ".Dumper \@ks;
    return wantarray ? @ks : $ks[0];
};


sub instances_authoritative {}; # signal that we can run our own instances method


# sub _tid2name {
#     my $tid = shift;
#     if ($tid =~ /cpan:(dist|mod|auth):(.+)/) {
#         return $2;
#     } else {
#         return '';
#     }
# }

around 'filter_topic' => sub {
    my $orig = shift;
    my $self = shift;
    my $bu   = $self->baseuri;
    my $out  = shift;
    my $t    = shift;

    if ($t =~ /(distribution|module|author):(.+)/) {
        my $name = $2;
        push @$out, [ _ca ($self,
                           kind    => TM2->NAME,
                           scope   => 'us',
                           type    => 'name',
                           roles   => [ 'topic', 'value' ],
                           players => [ $t,      TM2::Literal->new ($name) ]) ] ;
    } else {
	$TM2::log->warn ("unhandled axis topic $t");
    }
};

around 'filter_value' => sub {
    my $orig = shift;
    my $self = shift;

    my $out  = shift;
    my $v    = shift;

    my $h = TM2::DBI->handle( $self->dbi );
    # only support name values, ATM
    {
	my $sth = $h->prepare( qq{SELECT dist_id FROM dists WHERE dist_name = ? }) 
	    or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
	$sth->execute ($v->[0])
	    or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
	while (my ($did) = $sth->fetchrow_array) {
            push @$out, [ _ca ($self,
                               kind    => TM2->NAME,
                               scope   => 'us',
                               type    => 'name',
                               roles   => [ 'topic', 'value' ],
                               players => [ 'distribution:'.$v->[0],      $v ]) ] ;
	}
    }
    {
	my $sth = $h->prepare( qq{SELECT mod_name FROM mods WHERE mod_name = ? }) 
	    or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
	$sth->execute ($v->[0])
	    or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
	while (my ($did) = $sth->fetchrow_array) {
            push @$out, [ _ca ($self,
                               kind    => TM2->NAME,
                               scope   => 'us',
                               type    => 'name',
                               roles   => [ 'topic', 'value' ],
                               players => [ 'module:'.$v->[0],      $v ]) ] ;
	}
    }
    {
	my $sth = $h->prepare( qq{SELECT cpanid FROM auths WHERE cpanid = ? }) 
	    or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
	$sth->execute ($v->[0])
	    or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
	while (my ($did) = $sth->fetchrow_array) {
            push @$out, [ _ca ($self,
                               kind    => TM2->NAME,
                               scope   => 'us',
                               type    => 'name',
                               roles   => [ 'topic', 'value' ],
                               players => [ 'author:'.$v->[0],      $v ]) ] ;
	}
    }
};

around 'filter_topic_type' => sub {
    my $orig = shift;
    my $self = shift;
    my $out  = shift;
    my $t    = shift;
    my $ty   = shift;

    if ($t =~ /(distribution|module|author):(.+)/) {
        my $name = $2;
        if ($ty eq 'name') {
            push @$out, [ _ca ($self,
                               kind    => TM2->NAME,
                               scope   => 'us',
                               type    => 'name',
                               roles   => [ 'topic', 'value' ],
                               players => [ $t,      TM2::Literal->new ($name) ]) ] ;
        } else {
	    $TM2::log->warn ("unhandled axis type $ty topic $t");
        }
    }
};

around 'filter_iplayer_iroles' => sub {
    my $orig = shift;
    my $self = shift;
    my $bu   = $self->baseuri;
    my $out  = shift;
    my $ip   = shift;
    my @ir   = @_;

    my $h = TM2::DBI->handle( $self->dbi );

    if ($ip =~ /distribution:(.+)/) {
        my $name = $1;

#warn "XXXX $ip $name".Dumper \@ir;

        if (grep { $_ eq 'cpan:distribution' } @ir) { # if we want the involvement in modules

            my $sth1 = $h->prepare( qq{SELECT dist_id FROM dists WHERE dist_name = ? }) 
                or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
            my $sth2 = $h->prepare( qq{SELECT mod_name FROM mods WHERE dist_id = ? }) 
                or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");

            $sth1->execute ($name)
                or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
            while (my ($did) = $sth1->fetchrow_array) {  # collect all dists with that name (could be several)
#warn "XXXX $did";
		$sth2->execute ($did);
                while (my ($mname) = $sth2->fetchrow_array) {  # collect all dists with that name (could be several)
#warn "XXXX $did -> $mname";

                    push @$out, [ _ca ($self,
                                       kind    => TM2->ASSOC,
                                       scope   => 'us',
                                       type    => 'containment',
                                       roles   => [ 'cpan:distribution', 'cpan:module' ],
                                       players => [ $ip,                 'module:'.$mname ]) ] ;
                }
            }
	}

    } elsif ($ip =~ /module:(.+)/) {
        my $name = $1;

#warn "XXXX $ip $name".Dumper \@ir;

        if (grep { $_ eq 'cpan:module' } @ir) { # if we want the involvement in distributions

            my $sth1 = $h->prepare( qq{SELECT dist_id FROM mods WHERE mod_name = ? }) 
                or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
            my $sth2 = $h->prepare( qq{SELECT dist_name FROM dists WHERE dist_id = ? }) 
                or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");

            $sth1->execute ($name)
                or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
            while (my ($did) = $sth1->fetchrow_array) {  # collect all dists with that name (could be several)
#warn "XXXX $did";
		$sth2->execute ($did);
                while (my ($dname) = $sth2->fetchrow_array) {  # collect all dists with that name (could be several?)
#warn "XXXX $did -> $dname";
                    push @$out, [ _ca ($self,
                                       kind    => TM2->ASSOC,
                                       scope   => 'us',
                                       type    => 'containment',
                                       roles   => [ 'cpan:distribution',    'cpan:module' ],
                                       players => [ 'distribution:'.$dname, $ip ]) ] ;
                }
            }
	}

    } elsif ($ip =~ /author:(.+)/) {
        my $name = $1;

#warn "XXXX $ip $name".Dumper \@ir;

        if (grep { $_ eq 'cpan:author' } @ir) { # if we want the involvement in distributions
# TABLE auths (auth_id INTEGER NOT NULL PRIMARY KEY, email TEXT, fullname VARCHAR(40) NOT NULL, cpanid VARCHAR(20) NOT NULL);
# TABLE dists (dist_id INTEGER NOT NULL PRIMARY KEY, dist_vers VARCHAR(20), dist_dslip VARCHAR(5), dist_name VARCHAR(90) NOT NULL, auth_id INTEGER NOT NULL, dist_abs TEXT, dist_file VARCHAR(110) NOT NULL);
            my $sth1 = $h->prepare( qq{SELECT auth_id FROM auths WHERE cpanid = ? }) 
                or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
            my $sth2 = $h->prepare( qq{SELECT dist_name FROM dists WHERE auth_id = ? }) 
                or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");

            $sth1->execute ($name)
                or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
            while (my ($aid) = $sth1->fetchrow_array) {  # collect all dists with that name (could be several)
#warn "XXXX $did";
		$sth2->execute ($aid);
                while (my ($dname) = $sth2->fetchrow_array) {  # collect all dists with that name (could be several?)
#warn "XXXX $did -> $dname";
                    push @$out, [ _ca ($self,
                                       kind    => TM2->ASSOC,
                                       scope   => 'us',
                                       type    => 'authorship',
                                       roles   => [ 'cpan:distribution',    'cpan:author' ],
                                       players => [ 'distribution:'.$dname, $ip ]) ] ;
                }
            }
	}

    } else {
	$TM2::log->warn ("unhandled axis player $ip and roles ".Dumper \@ir);
    }

};

around 'filter_instance' => sub {
    my $orig = shift;
    my $self = shift;
    my $bu   = $self->baseuri;
    my $out  = shift;
    my $i    = shift;

    if ($i =~ /distribution:/) {
        push @$out, [ _ca ($self,
                           scope   => 'us',
                           type    => 'isa',
                           roles   => [ 'class',             'instance' ],
                           players => [ 'cpan:distribution', $i ]) ] ;

    } elsif ($i =~ /author:/) {
        push @$out, [ _ca ($self,
                           scope   => 'us',
                           type    => 'isa',
                           roles   => [ 'class',             'instance' ],
                           players => [ 'cpan:author',       $i ]) ] ;
#        push @$out, [ 'cpan:author' ];

    } elsif ($i =~ /module:/) {
        push @$out, [ _ca ($self,
                           scope   => 'us',
                           type    => 'isa',
                           roles   => [ 'class',             'instance' ],
                           players => [ 'cpan:module',       $i ]) ] ;

    } else {
        $self->$orig ($out, $i);
    }
    return $out;
};


around 'filter_class' => sub {
    my $orig = shift;
    my $self = shift;
    my $bu   = $self->baseuri;
    my $out  = shift;
    my $c    = shift;

    my $h = TM2::DBI->handle( $self->dbi );

    if ($c eq 'cpan:distribution') {
        $TM2::log->warn ("unwise to enumerate all distributions into memory; maybe use a tied tuple sequence")
            unless (tied @$out);  # memory enumeration we do not want

        my $sth = $h->prepare( qq{SELECT dist_name FROM dists $LIMIT}) 
            or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
        $sth->execute
            or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
        while (my ($name) = $sth->fetchrow_array) {
            push @$out, [ _ca ($self,
                               scope   => 'us',
                               type    => 'isa',
                               roles   => [ 'class',             'instance' ],
                               players => [ 'cpan:distribution', 'distribution:'.$name ]) ] ;
        }

    } elsif ($c eq 'cpan:author') {
        my $sth = $h->prepare( q{SELECT cpanid FROM auths}) 
            or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
        $sth->execute
            or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
        while (my ($name) = $sth->fetchrow_array) {
            push @$out, [ _ca ($self,
                                   scope   => 'us',
                                   type    => 'isa',
                                   roles   => [ 'class',       'instance' ],
                                   players => [ 'cpan:author', 'author:'.$name ]) ];
        }
        
    } elsif ($c eq 'cpan:module') {
        $TM2::log->warn ("unwise to enumerate all modules into memory; maybe use a tied tuple sequence")
            unless (tied @$out);  # memory enumeration we do not want

        my $sth = $h->prepare( qq{SELECT mod_name FROM mods $LIMIT}) 
            or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
        $sth->execute
            or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
        while (my ($name) = $sth->fetchrow_array) {
            push @$out, [ _ca ($self,
                               scope   => 'us',
                               type    => 'isa',
                               roles   => [ 'class',       'instance' ],
                               players => [ 'cpan:module', 'module:'.$name ]) ];
        }
    } else {
        $self->$orig ($out, $c);
    }
#    warn "done generating "; sleep 5;
    return $out;
};

# we are just not telling the whole truth here
# sub asserts {
#     $TM2::log->logdie ("unwilling to enumerate all CPAN assertions");
# }


#-- SQL fiddling ------------------------------------------------------------------------

my %TABLES = ('dists' => 'dist_name', 'mods' => 'mod_name', 'auths' => 'cpanid');

sub _lookup_name {
    my $dbh = shift;
    my $tbl = shift;
    my $col = $TABLES{$tbl};
    my $sth = $dbh->prepare("SELECT * FROM $tbl WHERE $col = ?");
    my $name = shift;
    eval {
        $sth->execute ($name);
    }; if ($@) {
        $TM2::log->warn ($@);
        return undef;
    }
    return $sth->fetchrow_hashref;
}

sub _lookup_all {
    my $dbh = shift;
    my $tbl = shift;
    my $col = $TABLES{$tbl};

    return $dbh->selectcol_arrayref("SELECT $col FROM $tbl");
}

sub _ca { # complete assertion
    my $tm = shift;
    my ($a) = $tm->normalize ( Assertion->new (@_) );
    return $a;
}

our $VERSION  = '0.5';

1;


# has 'handle' => (                # the database handle, will be opened at construction time
#     is => 'rw',
#     isa => 'DBI::db'
#     );

# has '_pid' => ( # sqlite handle depends on the process
#     is => 'rw',
#     isa => 'Int' );

sub _rehandle {
    my $self = shift;
    unless ($self->_pid == $$) {
#warn "different pid => rehandle";
	my $file   = $self->dbfile;
	my $handle = DBI->connect("dbi:SQLite:dbname=$file",undef,undef,$DBI_OPTIONS);
#warn "handle $handle";
	$self->handle ($handle);
#warn "handle set";
	$self->_pid ($$);
#warn "pid $$";
    }
    return $self->handle;
}

sub axis_class {
    $TM2::log->logdie ("unwilling to enumerate");
}

sub ch_axis_class_instance {
    my $self = shift;
    my $out = shift;
    my $bu = $self->baseuri;
    my $class = shift;

#warn "cpan axis class $class";

    use feature qw(switch);
    given ($class) {
	when ($bu . 'distribution') {
	    my $sth = $self->handle->prepare( qq{SELECT dist_name FROM dists $LIMIT}) 
		or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
	    $sth->execute
		or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
	    while (my ($name) = $sth->fetchrow_array) {
#print "$name\n";
		push @$out, [ $bu.'dist:'.$name ];
	    }
	}
	when ([ $bu . 'module', $bu . 'document' ]) {
	    my $sth = $self->handle->prepare( qq{SELECT mod_name FROM mods $LIMIT}) 
		or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
	    $sth->execute
		or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
	    while (my ($name) = $sth->fetchrow_array) {
		push @$out, [ $bu.'mod:'.$name ];
	    }
	}
	when ($bu . 'author') {
	    my $sth = $self->handle->prepare( q{SELECT cpanid FROM auths}) 
		or $TM2::log->logdie ("Can't prepare statement: $DBI::errstr");
	    $sth->execute
		or $TM2::log->logdie ("Can't execute statement: $DBI::errstr");
	    while (my ($name) = $sth->fetchrow_array) {
		push @$out, [ $bu.'cpanid:'.$name ];
	    }
	}
	default {
	    $TM2::log->logdie ("unhandled class '$class'");
	}
    }
    return $out;
}




1;

__END__

    {
	my $axes = $self->{'.axes'};                                       # shortcut
	$axes->{'memory'}->{'class=cpan:distribution&type=isa'} = sub {    # memory-based result: things which are instances of distribution
	    my $tm = shift;
	    my $bu = $self->baseuri;
	    return [
		map { [ $_ ] }
		$tm->normalize (
		    map { Assertion->new (scope   => 'us',
					  type    => 'isa',
					  roles   => [ 'class', 'instance' ],
					  players => [ 'distribution',  $_ ])
		    }
		    map { s/-/::/g; "${bu}dist:$_" }
		    @{ _lookup_all ($self->handle, 'dists') }
		    )
		];
	};
	delete $axes->{'memory'}->{'class=.*&type=isa'};                                  # deactivate all other memory class/instance
	$axes->{'disk'}->{'class=cpan:module&type=isa'} = sub {                           # disk-based result for distributions
	    my $tm  = shift;
	    my $out = shift;
	    my $bu  = $self->baseuri;
	    my $sth = $tm->handle->prepare ("SELECT mod_name FROM mods LIMIT 20");
	    $sth->execute;
	    while ( my @row = $sth->fetchrow_array ) {
		my ($a) = $tm->normalize (Assertion->new (scope   => 'us', type => 'isa',
							  roles   => [ 'class', 'instance' ],
							  players => [ 'module', "${bu}mod:$row[0]" ]) );
		push @$out, [ $a ];
	    }
	    return $out;
	};
    }








around 'match_forall' => sub {
    my $orig = shift;
    my $self = shift;
    my $bu   = $self->baseuri;
    
    my %query  = @_;
warn "# cpan match!!!" .Dumper \%query;

    $TM2::log->logdie (scalar __PACKAGE__ . ": unwilling to enumerate everything") unless %query;

#    my ($LOOKUP, $FQDN, $IP_ADDRESS, $INSTANCE, $CLASS) = $self->tids ('lookup', 'fqdn', 'ip-address', 'instance', 'class');

    my $axes = {
	'instance.type' => sub {
	    my $tm = shift;
	    my ($instance, $type) = @_;
	    if ($type eq 'isa') {
		use feature qw(switch);
		given ($instance) {
		    when (qr{:dist:}) {   # we defer from that that it is a distribution
			return _ca ($tm, 
				    scope   => 'us',
				    type    => 'isa',
				    roles   => [ 'class', 'instance' ],
				    players => [ 'distribution',  $instance ]);
		    }
		}
	    }
	    return $self->$orig (instance => $instance, type => 'isa');
	},
	'class=cpan:distribution&type=isa' => sub {
	},
	'class=cpan:module&type=isa' => sub {
	},

    };

    my @skeys = sort keys %query;                                                           # all fields make up the key
    my $skeys = join ('&', map { "$_=$query{$_}"} @skeys);

    warn $skeys;

    if (my $handler = $axes->{$skeys}) {
	return &$handler ($self);

    } else {
	return $self->$orig (%query);
    }

};

my $FQDN = qr{
                   (?:
	               (?! \d+ \. )
	               [a-zA-Z0-9]+ (?: -+ [a-zA-Z0-9]+ )*
	               \.
	           ) +
	           [a-zA-Z]{2,4}
                   }x;

my $IP = qr{((\d+)\.(\d+)\.(\d+)\.(\d+))};


around 'toplets' => sub {
    my $orig = shift;
    my $self = shift;
    my $bu   = $self->baseuri;

    $TM2::log->logdie (scalar __PACKAGE__ . ": unwilling to enumerate everything") unless @_;

    my @ks   = map {
	              ( /^${bu}ip:(.+)$/                               # smells like an IP address
		      ? [ $_, undef, [ 'urn:x-ip:'.$1 ] ]              # create a toplet with subject indicator on the fly
		      : ( /^${bu}fqdn:([\w\-\.]+)$/                    # smells like a name
		        ? [ $_, undef, [ 'urn:x-fqdn:'.$1 ] ]          # create toplet with subject indicator in the fly
		        : $self->$orig ($_)                                # if it is in the background map, then let's take that
		        )
                      ) 
 		} @_;

    return wantarray ? @ks : $ks[0];
};


around 'match_forall' => sub {
    my $orig = shift;
    my $self = shift;
    my $bu   = $self->baseuri;
    
    my %query  = @_;
#warn "# dns match!!!" .Dumper \%query;

    $TM2::log->logdie (scalar __PACKAGE__ . ": unwilling to enumerate everything") unless %query;

    my ($LOOKUP, $FQDN, $IP_ADDRESS, $INSTANCE, $CLASS) = $self->tids ('lookup', 'fqdn', 'ip-address', 'instance', 'class');

    if ($query{char}) {                                                   # want characteristics of something
	$_ = $query{irole};

	if (/ip:(.*)/) {                                                  # 123.123.123.123
	    return _ca ($self, 
			scope   => 'us',
			type    => 'name',
			kind    => TM2->NAME,
			roles   => [ 'thing', 'value' ],
			players => [ $_,      new TM2::Literal ("$1") ]);

	} elsif (/fqdn:(.+)$/ ) {                                         # www.rumsti.ramsti.de
	    return _ca ($self, 
			scope   => 'us',
			type    => 'name',
			kind    => TM2->NAME,
			roles   => [ 'thing', 'value' ],
			players => [ $_,      new TM2::Literal ("$1") ]);
	} else {
	    return $self->$orig (%query);
	}
    } elsif ($query{class} && $query{class} eq 'dns:fqdn' && $query{type} eq 'isa') {
	$TM2::log->logdie ("unwilling to enumerate all FQDNs");

    } elsif ($query{class} && $query{class} eq 'dns:ip-address' && $query{type} eq 'isa') {
	$TM2::log->logdie ("unwilling to enumerate all IP addresses (I could. But I won't)");

    } elsif ($query{instance} ||                                          # either we directly ask for instance assocs
             (defined $query{irole} &&
               $query{irole} eq $INSTANCE &&                              # or we have the role instance 
              ($query{instance} = $query{iplayer}))) {                    # and the player has the instance
	if ($query{instance} eq $bu.'localhost' ||
	    $query{instance} =~ /fqdn:(.+)$/) {
	    return _ca ($self,
			scope   => 'us',
			type    => 'isa',
			roles   => [ 'class', 'instance' ],
			players => [ $FQDN,  $query{instance} ]);

	} elsif ($query{instance} =~ /ip:(.+)/) {
	    return _ca ($self,
			scope   => 'us',
			type    => 'isa',
			roles   => [ 'class',     'instance' ],
			players => [ $IP_ADDRESS, $query{instance} ]);

	} else {
	    return $self->$orig (%query);
	}

    } elsif ($query{irole} && $query{iplayer}) {
                      # actually we do not look at the type here
                      # TODO: maybe we should
	if (($query{irole} eq $FQDN       && $query{iplayer} eq $bu.'localhost') ||
	    ($query{irole} eq $IP_ADDRESS && $query{iplayer} eq $bu.'ip:127.0.0.1')) {
	    return _ca ($self,
			scope   => 'us',
			type    => $LOOKUP,
			roles   => [ $FQDN,      $IP_ADDRESS ],
			players => [ 'localhost', '127.0.0.1' ]);
	    
	} elsif ($query{irole} eq $FQDN        && $query{iplayer} =~ /fqdn:(.+)/) {
	    my $host = $1;
	    my @a_records;
	    if (my $query = $res->search($host)) {
		foreach my $rr ($query->answer) {
		    next unless $rr->type eq "A";
		    push @a_records, _ca ($self,
					  scope   => 'us',
					  type    =>  $LOOKUP,
					  roles   => [ $FQDN,          $IP_ADDRESS ],
					  players => [ $query{iplayer},  $rr->address ]);
		}
	    }
	    return @a_records;

	} elsif ($query{irole} eq $IP_ADDRESS     && $query{iplayer} =~ /ip:(.+)/) {
	    my $ip = $1;
	    my @a_records;
	    if (my $query = $res->search($ip)) {
		foreach my $rr ($query->answer) {
		    next unless $rr->type eq "PTR";
		    push @a_records, _ca ($self,
					  scope   => 'us',
					  type    => $LOOKUP,
					  roles   => [ $FQDN,        $IP_ADDRESS ],
					  players => [ $rr->ptrdname, $query{iplayer} ]);
		}
	    }
	    return @a_records;
	} else {
	    return ();  # absolutely nothing else served here
	}

    } else {
	return $self->$orig (%query);
    }

};

around 'match_exists' => sub {
    my $orig = shift;
    my $self = shift;
    return 1 if $self->match_forall (@_);
};

our $VERSION  = '0.15';

1;


__END__


use TM;
use base qw(TM);

use TM::Literal;
use Data::Dumper;

# create a resolver, we need it at every lookup

=pod

=head1 NAME

TM::Virtual::DNS - Virtual Topic Map for DNS retrieval

=head1 SYNOPSIS

  # standalone
  use TM::Virtual::DNS;
  my $dns = new TM::Virtual::DNS;

  # forward lookup
  my @As = $tm->match_forall (irole   => $tm->tids ('fqdn'), 
                              iplayer => $tm->tids ('a.root-servers.net.'),
                              type    => $tm->tids ('lookup'));
  print map { TM::get_x_players ($dns, $_, $dns->tids ('ip-address') } @As;

  # reverse lookup
  my @PTRs = $tm->match_forall (irole   => $tm->tids ('ip-address'), 
                                iplayer => $tm->tids ('127.0.0.1'),
                                type    => $tm->tids ('lookup'));
  print map { TM::get_x_players ($dns, $_, $dns->tids ('fqdn') } @PTRs;

=head1 ABSTRACT

This class provides applications with a topicmapp-ish view of the DNS, the global domain name
service. As the content in the DNS can never be materialized, this topic map is virtual.

=head1 DESCRIPTION

This package overloads central methods of the L<TM> class.  In that, it provides access to DNS
information via the Topic Map paradigm. Hereby it uses a terminology from an onboard DNS ontology.

=head2 Ontology

While the map in its core functionality is virtual, it still is based on some fixed concepts, such
as I<IP address> or I<host name>. These are defined in the ontology which is represented textually
(in AsTMa= representation) within the string C<$ontology> (class property).

Whenever a DNS topic map is created, also this ontology is integrated, so that for the outside user
there is no visible distinction between topics declared in the ontology and topics (and
associations) created on-the-fly.

If you ever need the ontology, you can simply output it like so:

  perl -MTM::Virtual::DNS -e 'print $TM::Virtual::DNS::ontology;'

=cut


=pod

=head2 Identification

We introduce here our own URN x-namespaces to provide subject indicators for IP addresses and FQDN:

=head3 Subject Identifiers

=over

=item C<urn:x-ip> for IP addresses

Example:

     urn:x-ip:1.2.3.4

=item C<urn:x-dns> for DNS names

Example:

     urn:x-fqdn:www.google.com

=back

This package recognizes these subject indicators:

   print "yes" if $tm->tids (\ 'urn:x-ip:123.123.123.123');

=head3 Subject Locators

Obviously, there are no subject locators for IP addresses and FQDNs.

=head3 Local Identifiers

As local identifiers you can use IP addresses and FQDNs directly, they will be detected by
their syntactic structure:

   warn $tm->tids ('123.123.123.123');  # will create an absolutized local URI

   warn $tm->tids ('www.google.com');   # ditto


=head1 INTERFACE

=head2 Constructor

The constructor needs no arguments and instantiates a virtual map hovering over the DNS. For this
purpose the constructor also loads the background ontology (there is only a minimal overhead
involved with this). 

Example:

    my $dns = new TM::Virtual::DNS;

The following options are currently recognized:

=over

=item C<baseuri> (default: C<dns:localhost:>)

All local IDs in the virtual map will be prefixed with that baseuri.

=item C<nameservers> (default: whatever the local installation uses by default)

If this list reference is provided, the IP addresses in there will be used for name resolution.

B<Warning>: This feature cannot be properly tested automatically as many firewall setups prohibit
direct DNS access.

Example:

    my $dns = new TM::Virtual::DNS (nameservers => [ 1.2.3.4 ]);

=back

=cut

sub new {
    my $class   = shift;
    my %options = @_;
    $options{baseuri} ||= 'dns:localhost:';

    $res = Net::DNS::Resolver->new (nameservers => $options{nameservers})  # use explicit ones
	if $options{nameservers};                                          # if such an option existed

    my $self = bless $class->SUPER::new (%options), $class;

    use TM::Materialized::AsTMa;
    my $o = new TM::Materialized::AsTMa (inline  => $ontology,
					 baseuri => $self->{baseuri});
    $o->sync_in;                                                           # really load the ontology
    $self->melt ($o);                                                      # glue it to the map
    return $self;
}

=pod

=head2 Methods

This subclass of L<TM> overrides the following methods:

=over

=item B<tids>

This method expects a list of I<identification> parameters and will return a fully absolutized URI
for each of these. Apart from understanding the identifiers (as explained above), it should follow
the semantics of the mother class. It can also be used in list context.

=cut

sub tids {
    my $self = shift;
    my $bu   = $self->baseuri;

#warn "tids ".Dumper \@_;

    my @ks;
    foreach (@_) {
	if (!defined $_) {
	    return undef;
	} elsif (ref ($_)) {                                             # we got a subject indicator
	    my $si = $$_;
	    if ($si =~ /^urn:x-ip:((\d+)\.(\d+)\.(\d+)\.(\d+))$/ &&      # that indicating an IP address
		$2 < 256 && $3 < 256 && $4 < 256 && $5 < 256) {
		push @ks, $bu.'ip:'.$1;

	    } elsif ($si =~ /^urn:x-fqdn:([\w\-\.]+)$/ ) {               # that indicating a FQDN
		push @ks, $bu.'fqdn:'.$1;

	    } else {
		push @ks, $self->SUPER::tids ($_);
	    }

	# } elsif () {                                                   # in this world we NEVER have a subject locator

	} elsif ($self->{mid2iid}->{$_}) {                               # we got an absolute one
	    push @ks, $_;                                                # take that and run

	} elsif ($self->{mid2iid}->{$bu.$_}) {                           # simply prepending baseuri helps
	    push @ks, $bu.$_;                                            # take it

	} elsif (/^((\d+)\.(\d+)\.(\d+)\.(\d+))$/ &&                     # looks and
		 $2 < 256 && $3 < 256 && $4 < 256 && $5 < 256) {         # smells like an IP address
	    push @ks, $bu.'ip:'.$_;                                      # go with it

	} elsif (/^[\w\-\.]+$/ ) {                                       # cheapskate match for a name
	    push @ks, $bu.'fqdn:'.$_,                                    # take that

	} else {                                                         # do not know what it should be
	    push @ks, undef;
	}
    }
#warn "tids end ".Dumper \@ks;
    return wantarray ? @ks : $ks[0];
}

=pod

=item B<toplets>

This method returns toplet structures as described in L<TM>, either those of predefined concepts or
ones which are created on the fly if we are dealing with IP addresses or FQDNs.

This method can only deal with a list of local identifiers, not with search specifications. It will
refuse cooperation to enumerate the whole Internet when the list is empty.

TODO: support search specification

=cut

sub toplets {
    my $self = shift;
    my $bu   = $self->baseuri;

    $TM::log->logdie (scalar __PACKAGE__ . ": unwilling to enumerate everything") unless @_;

    my @ks   = map {
	            $self->tids ($_) 
                    ? $self->SUPER::toplets ($_)                       # if it is in the background map, then let's take that
                    : ( ( /^${bu}ip:(.+)$/ )                           # smells like an IP address
		      ? [ $_, undef, [ 'urn:x-ip:'.$1 ] ]              # create a toplet with subject indicator on the fly
		      : ( /^${bu}fqdn:([\w\-\.]+)$/                    # smells like a name
		        ? [ $_, undef, [ 'urn:x-fqdn:'.$1 ] ]          # create toplet with subject indicator in the fly
		        : undef                                        # no idea what this should be
		        )
		      )
		} @_;

    return wantarray ? @ks : $ks[0];
}

=pod

=item B<match_forall>

I<@assertions> = I<$tm>->match_forall (I<...search specification...>)

This method finds all assertions matching the search specification. Following axes are currently
supported:

=over

=item Code:char.irole

Return all assertions which are characteristics of the given topic. For IP addresses, there is one name
containing exactly the IP address as string. For FQDN also the string will be used as toplet name.

          'irole' => 'the toplet for which characteristics are sought',
          'char' => '1'

=item Code:instance.type

Returns all assertions where there are classes of a given toplet. For C<localhost> and all FQDN
toplets this is C<fqdn>, for C<127.0.0.1> and all IP addresses this is C<ip-address>.

=item Code:iplayer.irole.type  and Code irole.type

Returns all assertions where there is a C<lookup> assertion with the given toplet as player of the
given role. For IP addresses a reverse DNS lookup is done, for FQDNs a forward lookup.

=item Code:*

All other axes will only look into the underlying ontology.

=back

Examples:

   my @as = # forward lookup for localhost
   $tm->match_forall (irole   => $tm->tids ('fqdn'),   
                      iplayer => $tm->tids ('localhost'),
                      type    => $tm->tids ('lookup'))

   my @as = # forward lookup for one of the A servers
   $tm->match_forall (irole   => $tm->tids ('fqdn'),
                      iplayer => $tm->tids ('a.root-servers.net.'),
                      type    => $tm->tids ('lookup'));

   my @as = # reverse lookup
   $tm->match_forall (irole   => $tm->tids ('ip-address'),
                      iplayer => $tm->tids ($ip),
                      type    => $tm->tids ('lookup'))

=cut

sub _match_forall {
    my @x = _match_forall (@_);
    warn "returning form DNS match ".Dumper \@x;
    return @x;
}

sub match_forall {
    my $self   = shift;
    my $bu     = $self->baseuri;
    my %query  = @_;
#warn "# dns match!!!" .Dumper \%query;

    $TM::log->logdie (scalar __PACKAGE__ . ": unwilling to enumerate everything") unless %query;

    my ($LOOKUP, $FQDN, $IP_ADDRESS, $INSTANCE, $CLASS) = $self->tids ('lookup', 'fqdn', 'ip-address', 'instance', 'class');

    if ($query{char}) {                                                   # want characteristics of something
	$_ = $query{irole};

	if (/ip:(.*)/) {                                                  # 123.123.123.123
	    return $self->assert (
				  [ undef,
				    undef,
				    'name',
				    TM->NAME,
				    [ 'thing', 'value' ],
				    [ $_,      new TM::Literal ("$1") ],
				    ]);

	} elsif (/fqdn:(.+)$/ ) {                                         # www.rumsti.ramsti.de
	    return $self->assert (
				  [ undef,
				    undef,
				    'name',
				    TM->NAME,
				    [ 'thing', 'value' ],
				    [ $_,      new TM::Literal ("$1") ],
				    ]);

	} else {
	    return $self->SUPER::match_forall (%query);
	}
    } elsif ($query{instance} ||                                          # either we directly ask for instance assocs
             (defined $query{irole} &&
               $query{irole} eq $INSTANCE &&                              # or we have the role instance 
              ($query{instance} = $query{iplayer}))) {                    # and the player has the instance
	if ($query{instance} eq $bu.'localhost' ||
	    $query{instance} =~ /fqdn:(.+)$/) {
	    return $self->assert (Assertion->new (scope   => 'us',
						  type    => 'isa',
						  roles   => [ 'class', 'instance' ],
						  players => [ 'fqdn',  $query{instance} ]));

	} elsif ($query{instance} =~ /ip:(.+)/) {
	    return $self->assert (Assertion->new (scope   => 'us',
						  type    => 'isa',
						  roles   => [ 'class',     'instance' ],
						  players => [ 'ip-address', $query{instance} ]));

	} else {
	    return $self->SUPER::match_forall (@_);
	}


    } elsif ($query{irole} && $query{iplayer}) {
                      # actually we do not look at the type here
                      # TODO: maybe we should
	if (($query{irole} eq $FQDN       && $query{iplayer} eq $bu.'localhost') ||
	    ($query{irole} eq $IP_ADDRESS && $query{iplayer} eq $bu.'ip:127.0.0.1')) {
	    return $self->assert (Assertion->new (scope   => 'us',
						  type    => 'lookup',
						  roles   => [ 'fqdn',      'ip-address' ],
						  players => [ 'localhost', '127.0.0.1' ]));

	} elsif ($query{irole} eq $FQDN        && $query{iplayer} =~ /fqdn:(.+)/) {
	    my $host = $1;
	    my @a_records;
	    if (my $query = $res->search($host)) {
		foreach my $rr ($query->answer) {
		    next unless $rr->type eq "A";
		    push @a_records, $self->assert (Assertion->new (scope   => 'us',
								    type    => 'lookup',
								    roles   => [ 'fqdn',          'ip-address' ],
								    players => [ $query{iplayer},  $rr->address ]));
		}
	    }
	    return @a_records;

	} elsif ($query{irole} eq $IP_ADDRESS     && $query{iplayer} =~ /ip:(.+)/) {
	    my $ip = $1;
	    my @a_records;
	    if (my $query = $res->search($ip)) {
		foreach my $rr ($query->answer) {
		    next unless $rr->type eq "PTR";
		    push @a_records, $self->assert (Assertion->new (scope   => 'us',
								    type    => 'lookup',
								    roles   => [ 'fqdn',        'ip-address' ],
								    players => [ $rr->ptrdname, $query{iplayer} ]));
		}
	    }
	    return @a_records;
	} else {
	    return ();  # absolutely nothing else served here
	}

    } else {
	return $self->SUPER::match_forall (@_);
    }
}


=pod

=item B<match_exists>

See C<match_forall>.

=cut


=pod

=back

=head1 SEE ALSO

L<TM>

=head1 AUTHOR

Robert Barta, E<lt>drrho@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 200[3568] by Robert Barta

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut


1;

__END__

 =item B<toplets>

I<@toplets> = I<$tm>->toplets (I<@list_of_tids>)

This method implements the abstract one given in L<TM::Retrieve>.  It
(dynamically) generates toplets based on the provided (list of) topic
identifier(s).

You can try to omit the parameters, but then you will force the
package to give you all things it finds in the DNS. Not likely to
happen :-)

# TODO: Maybe later.

Examples:

   $localhost = $tm->toplets  ('localhost');
   $google    = $tm->toplets  ('urn:x-fqdn:www.google.com');
   $firewall  = $tm->toplets  ('urn:x-ip:192.168.0.1');

 =cut

sub xxxxxxxxtoplets {
  my $self = shift;
  return $self->{store}->toplets (@_);
}

 =pod



 =item B<maplets>

I<@maplets> = I<$tm>->maplets (I<$template>)

This method implements that described in L<TM::Access>. It dynamically
generates maplets according to the template passed in. It returns a
list of maplets matching the template search specification. It
supports only the following templates:

  TemplateFTypeFMember

Otherwise an exception is raised. If a search would result in a long
list, the method will raise an exception.

 =cut

sub xxxxxxxxxxxxxxxmaplets  {
  my $self     = shift;
  my $template = shift;

  my $ref_template = ref ($template);

##warn "dns maplet, resolving ".Dumper $template;

  if ($ref_template eq 'TemplateWildcard') {
    die "unwilling to enumerated all maplets";
  } elsif ($ref_template eq 'TemplateIPlayer') {       return _make_isa_s      ($template->iplayer),
							      _make_instance_s ($template->iplayer),
							      _make_forward_s  ($template->iplayer),
							      _make_reverse_s  ($template->iplayer);

  } elsif ($ref_template eq 'TemplateIPlayerIRole') {
    if ($template->irole eq 'fqdn') {                  return _make_forward_s  ($template->iplayer);
    } elsif ($template->irole eq 'ip-address') {       return _make_reverse_s  ($template->iplayer);
    } elsif ($template->irole eq 'class') {            return _make_instance_s ($template->iplayer);
    } elsif ($template->irole eq 'instance') {         return _make_isa_s      ($template->iplayer);
    } else { # ignore all others
    }

  } elsif ($ref_template eq 'TemplateIPlayerType') {
    if ($template->type eq 'is-a') {                   return _make_isa_s      ($template->iplayer),
							      _make_instance_s ($template->iplayer);
    } elsif ($template->type eq 'has-lookup') {        return _make_forward_s  ($template->iplayer),
							      _make_reverse_s  ($template->iplayer);
    } else { # ignore
    }

  } elsif ($ref_template eq 'TemplateIPlayerIRoleType') {
    if ($template->type eq 'is-a' &&
        $template->irole eq 'class') {                 return _make_instance_s ($template->iplayer);
    } elsif ($template->type eq 'is-a' &&
             $template->irole eq 'instance') {         return _make_isa_s      ($template->iplayer);
    } elsif ($template->type eq 'has-lookup' &&
	     $template->irole eq 'fqdn') {             return _make_forward_s  ($template->iplayer);
    } elsif ($template->type eq 'has-lookup' &&
	     $template->irole eq 'ip-address') {       return _make_reverse_s  ($template->iplayer);
    } else {
      # ignore
    }

##  } elsif ($ref_template eq 'Maplet') {
  } else {
    die "template '$ref_template' not implemented";
  }
}

 sub _make_isa_s {
  my $tid = shift;

  if ($tid eq 'localhost') {
    return (new Maplet (scope   => $TM::PSI::US,
			type    => 'is-a',
			roles   => [ 'instance',  'class' ],
			players => [ $tid,        'fqdn' ]));

  } elsif ($tid =~ /^ip-((\d+)-(\d+)-(\d+)-(\d+))$/) {
    return (new Maplet (scope   => $TM::PSI::US,
			type    => 'is-a',
			roles   => [ 'instance',  'class' ],
			players => [ $tid,        'ip-address' ]));

  } elsif ($tid =~ /^([\w-]+)$/) {
    return (new Maplet (scope   => $TM::PSI::US,
			type    => 'is-a',
			roles   => [ 'instance',  'class' ],
			players => [ $tid,        'fqdn' ]));
  } else {
    return (); # not known -> so no assoc
  }
}

sub _make_instance_s {
  my $tid = shift;

  if ($tid eq 'fqdn') {
    die "unwilling to enumerate instances";
  } elsif ($tid eq 'ip-address') {
    die "unwilling to enumerate instances";
  } else {
    return ();
  }
}

 =pod


      } elsif ($_ =~ /^((\d+)\.(\d+)\.(\d+)\.(\d+))$/ &&
	       $2 < 256 && $3 < 256 && $4 < 256 && $5 < 256) {
	  push @l, new Toplet (lid   => $self->{baseuri}.ip2tid ($_),
			       sids  => [ "urn:x-ip:$1" ],
			       chars => [ new Characteristic (lid   => undef,
							      scope => $self->{ontology}->tids ('us'),
							      type  => $self->{ontology}->tids ('has-basename'),
							      kind  => TM::Retrieve::KIND_BN, 
							      value => $1 ) ]);
	  
      } elsif ($_ =~ /^ip-((\d+)-(\d+)-(\d+)-(\d+))$/ &&
	       $2 < 256 && $3 < 256 && $4 < 256 && $5 < 256) {
	  push @l, new Toplet (lid   => $self->{baseuri}.$_,
			       sids  => [ "urn:x-ip:$2.$3.$4.$5" ],
			       chars => [ new Characteristic (lid   => undef,
							      scope => $self->{ontology}->tids ('us'),
							      type  => $self->{ontology}->tids ('has-basename'),
							      kind  => TM::Retrieve::KIND_BN, 
							      value => "$2.$3.$4.$5" ) ]);

      } elsif ($_ =~ /^(.*?\.[\w-]+\.[\w-]+)$/ ) {  # www.rumsti-ramsti.de
	  push @l, new Toplet (lid   => $self->{baseuri}.host2tid ($_),
			       sids  => [ "urn:x-dns:$_" ],
			       chars => [ new Characteristic (lid   => undef,
							      scope => $self->{ontology}->tids ('us'),
							      type  => $self->{ontology}->tids ('has-basename'),
							      kind  => TM::Retrieve::KIND_BN, 
							      value => $1 ) ]);

      } elsif ($_ =~ /^([\w-]+)$/ ) {               # www-rumsti--ramsti-de
	  push @l, new Toplet (lid   => $self->{baseuri}.$_,
			       sids  => [ "urn:x-dns:".tid2host ($_) ],
			       chars => [ new Characteristic (lid   => undef,
							      scope => $self->{ontology}->tids ('us'),
							      type  => $self->{ontology}->tids ('has-basename'),
							      kind  => TM::Retrieve::KIND_BN, 
							      value => tid2host ($_) ) ]);

      } else {                                                        # whatta crap is that?
	  push @l, undef;
      }


sub ip2tid {
  my $tid = shift;
  $tid =~ s/\./\-/g;
  return "ip-$tid";
}

sub tid2ip {
  my $tid = shift;
  my ($ip) = $tid =~ /^ip-(.+)/;
  $ip =~ s/-/./g;
  return $ip;
}

sub host2tid {
  my $x = shift;
  $x =~ s/-/--/g;
  $x =~ s/\./\-/g;
  return $x;
}

sub tid2host {
  my $x = shift;
  $x =~ s/-(?!-)/./g;
  $x =~ s/--/-/g;
  return $x;
}



__END__
