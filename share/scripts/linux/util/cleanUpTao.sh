#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables

killall Naming_Service 
killall Exe_server 

foreach file (Units/${CFDHOSTTYPE}/*UnitApp)
   echo "Killing" $file
   killall $file 
end
