#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables

foreach file (*.vtk)
echo ""
cp -p ${file} ${file}_ObsoleteFormat
makeVtkSurface ${file} ${file}_surf << EOF
y
0
0.7
EOF
removeVtkCellsOutsideBox ${file}_surf ${file}_surf -.15 .15 -.15 .15 .0001 2.59 << EOF
y
EOF
convertSurfaceFileToStl ${file}_surf ${file}.stl << EOF
y
EOF
end

