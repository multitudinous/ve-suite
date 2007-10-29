#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables

foreach file (*.vtk)
   echo ""
   cp -p ${file} ${file}_preTransform
   transformVTK $file $file 90 0 0 0 0 0
   # transformVTK rotX rotY rotZ transX transY transZ
end

