#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# used for transient data processing: to concatenate many planes into one polydata object

foreach file (*.vtk)
   echo "" $file
   convertCellDataToPointData $file new_$file
end

