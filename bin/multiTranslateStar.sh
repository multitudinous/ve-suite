#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables

foreach file (*.vrt)
   echo ""
   echo $file
   translateToVtk 2 $file 1 0.0025 0.0025 0.0025 0 0
   # above means translate all sets of star.cel, star.usr, star.vrt using custom scale factor of 0.0025.  This code works as of 2003/07/24.
end

