#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables

foreach file (*.vtk)
   echo ""
   cp -p ${file} ${file}_ObsoleteFormat
   moveFieldToPointData $file $file
end

