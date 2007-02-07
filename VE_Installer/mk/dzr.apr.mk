ifeq ($(CFDHOSTTYPE), Darwin)
   EXTRA_INCLUDES+= $(shell flagpoll apr-1 apr-util-1 --cflags-only-I)
   EXTRA_CXXFLAGS+=  $(shell flagpoll apr-1 apr-util-1 --cflags-only-other) 
   EXTRA_LIBS+= $(shell flagpoll apr-1 apr-util-1 --libs)
   DSO_PLUGIN_DEPS+= $(shell flagpoll apr-1 apr-util-1 --libs)
else
   EXTRA_INCLUDES+= $(shell apu-config --includes) $(shell apr-config --includes)
   EXTRA_CXXFLAGS+=  $(shell apr-config --cppflags) 
   EXTRA_LIBS+= $(shell apu-config --link-ld --libs) $(shell apr-config --link-ld --libs)
   DSO_PLUGIN_DEPS+= $(shell apu-config --link-ld --libs)
endif

