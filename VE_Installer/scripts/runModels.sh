#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables

foreach file (Units/${CFDHOSTTYPE}/*UnitApp)
   echo ""
   $file -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService &
   sleep 1
end

