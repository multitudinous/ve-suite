CXX = $(shell wx-config --cxx)
WX_FLAGS = $(shell wx-config --cxxflags)
WX_LIBS = $(shell wx-config --libs)

PLATFORM = LINUX
COMPILER = g++
GCC = yes
GXX = yes
CXXFLAGS =   -g -w -O  -DAPP_NO_THREADS -DXML_USE_NO_THREADS  -I../
CFLAGS =   -g -w -O  -DAPP_NO_THREADS -DXML_USE_NO_THREADS  
LDFLAGS =    
THREADS = none
EXTRA_LIBS =  
PREFIX = /usr/local

#include ${XERCESCROOT}/version.incl
include ../Makefile.incl

PLUGIN = ../libOPPD.so
OBJECTS = OPPD.o \
	OPPD_UI.o \
   OPPD_UI_entries.o \
   OPPD_UI_outputs.o \
   OPPD_UI_tabs.o \
   OPPD_UI_constants.o

.SUFFIXES : .o .cpp

.cpp.o :
	$(CXX) -g -c ${INCLUDES} ${CMP} ${WX_FLAGS} -D WXUSING_PLUGIN_DLL -o $@ $<

all: $(PLUGIN)

$(PLUGIN): $(OBJECTS)
#
	$(CXX) -g -shared -o $(PLUGIN) $(OBJECTS) 
clean:
	rm -rf *.o $(PLUGIN) $(OBJECTS) 

