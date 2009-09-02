# need to point to home of 64-bit vtk install
DEPS_HOME = ${HOME}/vr/dependencies

# C++ compiler 
CXX = g++

# Standard flags for C++ 
CXXFLAGS ?= -arch x86_64 -isysroot /Developer/SDKs/MacOSX10.5.sdk -g

# Standard preprocessor flags (common for CC and CXX)
# ENGINE is for TecUtilParentLockXXX
# UNIXX and ENGINE are needed for Widget of TECUTILS.h. See MASTER.h.
# DARWIN will activate UNIXX
CPPFLAGS ?= -DENGINE -DDARWIN -DMAC64

# Standard linker flags 
LDFLAGS ?= -arch x86_64 -isysroot /Developer/SDKs/MacOSX10.5.sdk -g \
        -L${TECSDKHOME}/lib/macix64.105 \
        -ltpsdkbase -ltpsdkintegrationmanager -ltptoolbox \
        -L${DEPS_HOME}/lib64/vtk-5.4 \
        -lvtkCommon -lvtkFiltering -lvtkIO

# wxWidgets-related C++ flags
WX_CXXFLAGS ?= $(shell wx-config --cxxflags)
WX_LDFLAGS ?= $(shell wx-config --libs gl,core,base)

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
                    -I${DEPS_HOME}/include/vtk-5.4 \
                     $(WX_CXXFLAGS) $(CPPFLAGS) $(CXXFLAGS)
CPPDEPS = -MT$@ -MF$(shell echo $@ | sed -e 's,\.o$$,.d,') -MD -MP

### Targets: ###

all: $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) Makefile
	$(CXX) -o $@ $(OBJECTS) $(WX_LDFLAGS) $(LDFLAGS)

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
