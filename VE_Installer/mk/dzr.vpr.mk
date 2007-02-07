EXTRA_CXXFLAGS+= $(shell flagpoll vpr --cflags-only-other )
EXTRA_LIBS+= $(shell flagpoll vpr --libs) 
EXTRA_INCLUDES+= $(shell flagpoll vpr --cflags-only-I)
DSO_PLUGIN_DEPS+= $(shell flagpoll vpr --libs) 
