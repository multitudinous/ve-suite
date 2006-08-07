ifeq ($(CFDHOSTTYPE), Darwin)
   EXTRA_INCLUDES+= $(shell apu-1-config --includes) $(shell apr-1-config --includes)
   EXTRA_CXXFLAGS+=  $(shell apr-1-config --cppflags) 
   EXTRA_LDFLAGSS+= $(shell apu-1-config --link-ld --libs)
   DSO_PLUGIN_DEPS+= $(shell apu-1-config --link-ld --libs)
else
   EXTRA_INCLUDES+= $(shell apu-config --includes) $(shell apr-config --includes)
   EXTRA_CXXFLAGS+=  $(shell apr-config --cppflags) 
   EXTRA_LDFLAGSS+= $(shell apu-config --link-ld --libs)
   DSO_PLUGIN_DEPS+= $(shell apu-config --link-ld --libs)
endif

