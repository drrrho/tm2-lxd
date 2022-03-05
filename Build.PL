use strict;
use warnings;
use Module::Build;

$ENV{'PERL_MB_OPT'} = '--install-base=/usr'; # I could not make this work via the parameters below :/
$ENV{'DEB_BUILD_OPTIONS'}='nocheck';

use lib qw(../tm2_base/lib ../templescript/lib);

my $debian = Module::Build->subclass(
    class => "Module::Build::Debian",
    code => <<'SUBCLASS' );

sub DEBVERSION {
    return 1;
}

sub ACTION_debian {
    my $self = shift;

    $self->depends_on("distdir");

    my $version = $self->dist_version;
    my $debversion = $self->DEBVERSION;
    $self->do_system(qq{ export DEB_BUILD_OPTIONS=nocheck ; dh-make-perl --no-network --build --version=$version-$debversion TM2-lxd-$version } );

}
sub ACTION_inject {
    my $self = shift;

    $self->depends_on("debian");

    my $version = $self->dist_version;
    my $debversion = $self->DEBVERSION;
    $self->do_system(qq{ scp libtm2-lxd-perl_${version}-${debversion}_all.deb packages.devc.at:packages/ } );
    foreach (qw(squeeze wheezy jessie stretch) ) {
        $self->do_system(qq{ ssh packages.devc.at reprepro -b /var/www/packages/data/dists/ includedeb $_ packages/libtm2-lxd-perl_${version}-${debversion}_all.deb });
    }
}
SUBCLASS

my $builder = $debian->new(
    module_name         => 'TM2::lxd',
    license             => 'artistic_2',
    dist_author         => q{Robert Barta <rho@devc.at>},
    dist_version_from   => 'lib/TM2/lxd.pm',
    release_status      => 'stable',
    configure_requires => {
        'Module::Build' => '0',
    },
    build_requires => {
        'Test::More' => '0',
        'Test::Exception' => 0,
        'Net::Async::WebService::lxd' => 0,
    },
    requires => {
        'TM2::TempleScript' => 0,
	'Data::Structure::Util' => 0,
        'Net::Async::WebService::lxd' => 0,
    },
    add_to_cleanup     => [ 'TM2-lxd-*' ],
    create_makefile_pl => 'traditional',
    onto_files          => {
	'ontologies/lxd.atm'       => 'ontologies/lxd.atm',
	'ontologies/lxd.ts'        => 'ontologies/lxd.ts',
	'ontologies/lxc.ts'        => 'ontologies/lxc.ts',
    },
    install_path        => { 'ontologies' => '/usr/share/templescript/ontologies' },
);

$builder->add_build_element('onto');
$builder->create_build_script();
