EXTRA_CXXFLAGS+= -D_OSG $(shell flagpoll osg --cflags-only-other )
EXTRA_INCLUDES+= $(shell flagpoll osg --cflags-only-I )
EXTRA_LIBS+= $(shell flagpoll osg --libs )
DSO_PLUGIN_DEPS+= $(shell flagpoll osg --libs )