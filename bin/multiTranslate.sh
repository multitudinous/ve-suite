#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables

foreach file (*.case)
   echo ""
   echo $file
   translateToVtk 4 $file 1 20 20 20 0 0
   # above means translate all ensight case files using custom scale factor of 20
   # translateToVtk 4 $file 2 0 0
   # above means translate all ensight case files using scale factor MetersToFeet
end

