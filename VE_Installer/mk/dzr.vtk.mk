################### vtk-specific (vtk4 and higher) Makefile ###################
EXTRA_INCLUDES+= -I${VTK_BASE_DIR}/include/vtk-5.0
EXTRA_INCLUDES+= -I${VE_SUITE_HOME}

EXTRA_LIBS+= -L${VTK_BASE_DIR}/lib -lvtkImaging -lvtkGraphics\
             -lvtkCommon -lvtkHybrid -lvtkIO -lvtkFiltering\
             -lvtkRendering -lvtkParallel -lvtkexpat\
            -lvtkpng -lvtktiff -lvtksys -lvtkjpeg\
            -lvtkexoIIc -lvtkftgl -lvtkfreetype -lvtkDICOMParser\
            -lvtkzlib -lvtkMPEG2Encode -lvtkNetCDF 

DSO_PLUGIN_DEPS+= -L${VTK_BASE_DIR}/lib -lvtkImaging -lvtkGraphics\
                  -lvtkCommon -lvtkHybrid -lvtkIO -lvtkFiltering\
                  -lvtkRendering -lvtkParallel -lvtkexpat\
                  -lvtkpng -lvtktiff -lvtksys -lvtkjpeg\
                  -lvtkexoIIc -lvtkftgl -lvtkfreetype -lvtkDICOMParser\
                  -lvtkzlib -lvtkMPEG2Encode -lvtkNetCDF 
#vtkActorToPf needs to know which version of VTK we are using:
EXTRA_CXXFLAGS+= -DVTK44 -DVTK_STREAMS_FWD_ONLY

