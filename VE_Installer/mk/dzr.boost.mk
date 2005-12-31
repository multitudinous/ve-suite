EXTRA_CXXFLAGS+=
ifeq ($(CFDHOSTTYPE), Darwin)
EXTRA_LIBS+= -L$(VJ_DEPS_DIR)/lib -lboost_filesystem-1_33
else
EXTRA_LIBS+= -L$(VJ_DEPS_DIR)/lib -lboost_filesystem-gcc-mt-1_33
endif
EXTRA_INCLUDES+= -I$(VJ_DEPS_DIR)/include
DSO_PLUGIN_DEPS+= $(EXTRA_LIBS)
