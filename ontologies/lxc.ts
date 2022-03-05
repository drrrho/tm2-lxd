# perl -I../net-async-webservice-lxd/lib/ -I../tm2-base/lib/ -I../templescript/lib/ -Ilib ../templescript/bin/ts --module=TM2::Virtual::lxd --ur-path=../templescript/ontologies/ --path=ontologies/ --map ontologies/lxc.ts --continuation --variable=endpoint=https://192.168.3.50:8443

%include file:lxd.ts

§ isa ts:stream
  isa ts:side-effect
return
    ( $endpoint ) |->> ts:fusion( lxd:host )
  |->> ts:locals( "$_" )


