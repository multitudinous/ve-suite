# Create the OS-dependent directory name
UNAME= $(shell uname -s)

# -----------------------------------------------------------------------------
# Build targets.
# -----------------------------------------------------------------------------
default: all

all: makeVtkSurface meshViewer convertVTK2Binary \
      convertVTK2Ascii transformVTK scaleVTK \
      removeVtkCellsOutsideBox convertSurfaceFileToStl \
      removeVtkPointData arrowCreator addNormals2Poly \
      moveFieldToPointData whatIsScalarRange \
      combineFluentParticleFile mergeVertices appendVTK \
      createMiniFlowdata convertCellDataToPointData

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
makeVtkSurface meshViewer convertVTK2Binary \
      convertVTK2Ascii transformVTK scaleVTK \
      removeVtkCellsOutsideBox convertSurfaceFileToStl \
      removeVtkPointData arrowCreator addNormals2Poly \
      moveFieldToPointData whatIsScalarRange \
      combineFluentParticleFile mergeVertices appendVTK \
      createMiniFlowdata convertCellDataToPointData ::
	mkdir -p $(VE_SUITE_HOME)/bin/$(UNAME)
	cd ./mk/$@ && $(MAKE) all

# -----------------------------------------------------------------------------
# Clean-up.
# -----------------------------------------------------------------------------
clean:	
	-cd ./mk/makeVtkSurface && $(MAKE) clean
	-cd ./mk/meshViewer && $(MAKE) clean
	-cd ./mk/convertVTK2Binary && $(MAKE) clean
	-cd ./mk/convertVTK2Ascii && $(MAKE) clean
	-cd ./mk/transformVTK && $(MAKE) clean
	-cd ./mk/scaleVTK && $(MAKE) clean
	-cd ./mk/removeVtkCellsOutsideBox && $(MAKE) clean
	-cd ./mk/convertSurfaceFileToStl && $(MAKE) clean
	-cd ./mk/removeVtkPointData && $(MAKE) clean
	-cd ./mk/arrowCreator && $(MAKE) clean
	-cd ./mk/addNormals2Poly && $(MAKE) clean
	-cd ./mk/moveFieldToPointData && $(MAKE) clean
	-cd ./mk/whatIsScalarRange && $(MAKE) clean
	-cd ./mk/combineFluentParticleFile && $(MAKE) clean
	-cd ./mk/mergeVertices && $(MAKE) clean
	-cd ./mk/appendVTK && $(MAKE) clean
	-cd ./mk/createMiniFlowdata && $(MAKE) clean
	-cd ./mk/convertCellDataToPointData && $(MAKE) clean

cleandepend:	
	-cd ./mk/makeVtkSurface && $(MAKE) cleandepend
	-cd ./mk/meshViewer && $(MAKE) cleandepend
	-cd ./mk/convertVTK2Binary && $(MAKE) cleandepend
	-cd ./mk/convertVTK2Ascii && $(MAKE) cleandepend
	-cd ./mk/transformVTK && $(MAKE) cleandepend
	-cd ./mk/scaleVTK && $(MAKE) cleandepend
	-cd ./mk/removeVtkCellsOutsideBox && $(MAKE) cleandepend
	-cd ./mk/convertSurfaceFileToStl && $(MAKE) cleandepend
	-cd ./mk/removeVtkPointData && $(MAKE) cleandepend
	-cd ./mk/arrowCreator && $(MAKE) cleandepend
	-cd ./mk/addNormals2Poly && $(MAKE) cleandepend
	-cd ./mk/moveFieldToPointData && $(MAKE) cleandepend
	-cd ./mk/whatIsScalarRange && $(MAKE) cleandepend
	-cd ./mk/combineFluentParticleFile && $(MAKE) cleandepend
	-cd ./mk/mergeVertices && $(MAKE) cleandepend
	-cd ./mk/appendVTK && $(MAKE) cleandepend
	-cd ./mk/createMiniFlowdata && $(MAKE) cleandepend
	-cd ./mk/convertCellDataToPointData && $(MAKE) cleandepend
