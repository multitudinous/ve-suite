EXTRA_CXXFLAGS+= $(shell flagpoll boost.filesystem --cflags-only-other) 
EXTRA_LIBS+= $(shell flagpoll boost.filesystem --libs)
DSO_PLUGIN_DEPS+= $(shell flagpoll boost.filesystem --libs)
EXTRA_INCLUDES+= $(shell flagpoll boost.filesystem --cflags-only-I)
