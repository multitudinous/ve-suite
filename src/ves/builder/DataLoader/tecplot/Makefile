# need to point to home of 64-bit vtk install
VTK_LIB_HOME = ${HOME}/vr/lib64/vtk-5.4
VTK_INCLUDE_HOME = ${HOME}/vr/dependencies/include/vtk-5.4

# boost is in the macports install
MACPORTS_HOME=/opt/local

# C++ compiler 
CXX = g++

# Standard flags for C++ 
CXXFLAGS ?= -arch x86_64 -isysroot /Developer/SDKs/MacOSX10.5.sdk -g -Wno-deprecated

# Standard preprocessor flags (common for CC and CXX)
# ENGINE is for TecUtilParentLockXXX
# UNIXX and ENGINE are needed for Widget of TECUTILS.h. See MASTER.h.
# DARWIN will activate UNIXX
CPPFLAGS ?= -DENGINE -DDARWIN -DMAC64

# Standard linker flags 
LDFLAGS ?= -arch x86_64 -isysroot /Developer/SDKs/MacOSX10.5.sdk -g \
        -L${TECSDKHOME}/lib/macix64.105 \
        -ltpsdkbase -ltpsdkintegrationmanager -ltptoolbox \
        -L${VTK_LIB_HOME} \
        -lvtkCommon -lvtkFiltering -lvtkIO

### Variables: ###

SOURCES=tecplotReader.cxx Manager.cxx ApplicationEventMonitor.cxx 
OBJECTS=$(SOURCES:.cxx=.o)
HEADERS=$(SOURCES:.cxx=.h)
EXECUTABLE=tecplotReader

COMPILE_CXXFLAGS =  -I. \
                    -I${TECSDKHOME}/include \
                    -I${TECSDKHOME}/include/sdk \
                    -I${TECSDKHOME}/include/sdk/integrationmanager \
                    -I${TECSDKHOME}/include/sdk/integrationmanager/services \
                    -I${TECSDKHOME}/include/toolbox \
                    -I${VTK_INCLUDE_HOME} \
                    -I${MACPORTS_HOME}/include \
                     $(CPPFLAGS) $(CXXFLAGS)
CPPDEPS = -MT$@ -MF$(shell echo $@ | sed -e 's,\.o$$,.d,') -MD -MP

### Targets: ###

all: $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) Makefile
	$(CXX) -o $@ $(OBJECTS) $(LDFLAGS)

#$(OBJECTS): $(HEADERS) makefile_mac

%.o: %.cxx
	$(CXX) -c -o $@ $(COMPILE_CXXFLAGS) $(CPPDEPS) $<

install: 

uninstall: 

clean: 
	rm -f ./*.o
	rm -f ./*.d
	rm -f tecplotReader

.PHONY: all install uninstall clean

# Dependencies tracking:
-include ./*.d
