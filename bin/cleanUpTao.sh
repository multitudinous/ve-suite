#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# used for transient data processing: to concatenate many planes into one polydata object

killall Naming_Service 
killall Exe_server 

foreach file (Units/${CFDHOSTTYPE}/*UnitApp)
   echo "Killing" $file
   killall $file 
end
