################### vtk-specific (vtk4 and higher) Makefile ###################
EXTRA_INCLUDES+= $(shell flagpoll vtk --cflags-only-I)

EXTRA_LIBS+= $(shell flagpoll vtk --libs) 

DSO_PLUGIN_DEPS+= $(shell flagpoll vtk --libs) 
 
#vtkActorToPf needs to know which version of VTK we are using:
EXTRA_CXXFLAGS+= $(shell flagpoll vtk --cflags-only-other)

