MAC_OS=10.6
MATLAB_ROOT=/Applications/MATLAB_R2011b.app

# C++ compiler 
CXX = g++

# Standard flags for C++ 
CXXFLAGS ?= -arch x86_64 -isysroot /Developer/SDKs/MacOSX${MAC_OS}.sdk -g -Wno-deprecated -Wall

# Standard preprocessor flags (common for CC and CXX)
CPPFLAGS ?= -DPRINT_HEADERS

# Standard linker flags 
LDFLAGS ?= -arch x86_64 -isysroot /Developer/SDKs/MacOSX${MAC_OS}.sdk -g \
        -L$(MATLAB_ROOT)/bin/maci64 -lmx -leng

### Variables: ###
SOURCES=controlModel.cxx
OBJECTS=$(SOURCES:.cxx=.o)
HEADERS=$(SOURCES:.cxx=.h)
EXECUTABLE=controlModel

COMPILE_CXXFLAGS =  -I. \
                    -I$(MATLAB_ROOT)/extern/include/ \
                     $(CPPFLAGS) $(CXXFLAGS)

CPPDEPS = -MT$@ -MF$(shell echo $@ | sed -e 's,\.o$$,.d,') -MD -MP

### Targets: ###

all: $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) Makefile
	$(CXX) -o $@ $(OBJECTS) $(LDFLAGS)
	rm -f ./*.o
	rm -f ./*.d

%.o: %.cxx
	$(CXX) -c -o $@ $(COMPILE_CXXFLAGS) $(CPPDEPS) $<

install: 

uninstall: 

clean: 
	rm -f ./*.o
	rm -f ./*.d
	rm -f $(EXECUTABLE)

.PHONY: all install uninstall clean

# Dependencies tracking:
-include ./*.d
