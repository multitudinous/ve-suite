################### vtk-specific (vtk4 and higher) Makefile ###################

EXTRA_INCLUDES+= -I${VTK_BASE_DIR}/include/vtk 

EXTRA_LIBS+= -L${VTK_BASE_DIR}/lib/vtk -lvtkImaging -lvtkGraphics\
               -lvtkCommon -lvtkHybrid -lvtkIO -lvtkFiltering\
               -lvtkRendering -lvtkParallel
