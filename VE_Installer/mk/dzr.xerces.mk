EXTRA_INCLUDES+= $(shell flagpoll xerces --cflags)
EXTRA_LIBS+= $(shell flagpoll xerces --libs)

DSO_PLUGIN_DEPS+= $(shell flagpoll xerces --libs)
