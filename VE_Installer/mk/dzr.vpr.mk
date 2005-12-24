EXTRA_CXXFLAGS+= $(shell vpr-config --cxxflags)
EXTRA_LIBS+= $(shell vpr-config --libs) $(shell vpr-config --extra-libs) 
EXTRA_INCLUDES+= $(shell vpr-config --includes)

DSO_PLUGIN_DEPS+= $(shell vpr-config --libs) $(shell vpr-config --extra-libs) 
