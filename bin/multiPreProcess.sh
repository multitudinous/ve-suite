#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# used for transient data processing: to concatenate many planes into one polydata object

foreach file (*.vtk)
   echo ""
   preprocessor $file POST_DATA 0 0 0 1 2 cuttingPlane.param 1
   #above line means only process planes, store output in POST_DATA dir, file 'cuttingPlane.param' specifies location of planes
   #preprocessor $file POST_DATA 0 0 0 1 1 0 3 4 1
   #above line means only process planes, let program choose location, 0 x-planes, 3 y-planes, 4 z-planes
end

