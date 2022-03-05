%include file:lxd.atm

lxd:host

isa implementation @ lang:perl :
   class        : lxd:host
   ts:software  : urn:x-perl:TM2::Virtual::lxd

§ isa ts:converter
ts:mimetype @ ts:input  : "templescript/hash"
ts:mimetype @ ts:output : "lxd/project"
returns """
  return bless $_[0], "templescript/hash;mime=lxd/project";
""" ^^ lang:perl !


