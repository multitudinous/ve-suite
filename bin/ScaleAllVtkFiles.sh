#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables

foreach file (*.vtk)
   echo ""
   cp -p ${file} ${file}_preScale
   # below line employs a custom scale
   scaleVTK $file $file 1 1.5 1.5 1.5
   # below line employs built-in scale meters to feet
   #scaleVTK $file $file 2
end

