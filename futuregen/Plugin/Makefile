CXX = $(shell wx-config --cxx)
WX_FLAGS = $(shell wx-config --cxxflags)
WX_LIBS = $(shell wx-config --libs)

PLATFORM = LINUX
COMPILER = g++
GCC = yes
GXX = yes
CXXFLAGS =   -g -w -O  -DAPP_NO_THREADS -DXML_USE_NO_THREADS  
CFLAGS =   -g -w -O  -DAPP_NO_THREADS -DXML_USE_NO_THREADS  
LDFLAGS =    
THREADS = none
EXTRA_LIBS =  
PREFIX = /usr/local

#include ${XERCESCROOT}/version.incl
include Makefile.incl

PLUGIN = libZplugin.so
OBJECTS = string_ops.o package.o interface.o UIDialog.o Plugin_base.o TexTable.o TextResultDialog.o FinancialDialog.o

.SUFFIXES : .o .cpp

.cpp.o :
	$(CXX) -g -c  ${INCLUDES} ${CMP} ${WX_FLAGS} -D WXUSING_PLUGIN_DLL -o $@ $<

all: $(PLUGIN)

$(PLUGIN): $(OBJECTS)
#
	$(CXX) -g -shared -o $(PLUGIN) $(OBJECTS) $(WX_LIBS)
#	$(CXX) -g -o $(PLUGIN) $(OBJECTS) $(WX_LIBS)
clean:
	rm -rf *.o $(PLUGIN) Debug 
