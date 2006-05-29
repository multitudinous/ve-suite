EXTRA_CXXFLAGS+= $(shell wx-config --cxxflags) -DWXUSINGDLL
EXTRA_LIBS+= $(shell wx-config --libs)

CFDUNAME = $(shell uname)
ifeq (${CFDUNAME},IRIX64)
EXTRA_LIBS+= -L/usr/freeware/lib32 
endif
DSO_PLUGIN_DEPS+= $(shell wx-config --libs) -DWXUSINGDLL

