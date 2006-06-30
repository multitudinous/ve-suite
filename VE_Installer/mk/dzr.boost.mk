EXTRA_CXXFLAGS+=
ifeq ($(CFDHOSTTYPE), Darwin)
EXTRA_LIBS+= -L$(VJ_DEPS_DIR)/lib -lboost_filesystem-1_33_1
DSO_PLUGIN_DEPS+= -L$(VJ_DEPS_DIR)/lib -lboost_filesystem-1_33_1
else
ifeq ($(shell uname -m), x86_64)
EXTRA_LIBS+= -L$(VJ_DEPS_DIR)/lib64 -lboost_filesystem-gcc-mt-1_33_1
DSO_PLUGIN_DEPS+= -L$(VJ_DEPS_DIR)/lib64 -lboost_filesystem-gcc-mt-1_33_1
else
   ifeq ($(shell uname -m), ia64)
   EXTRA_LIBS+= -L$(VJ_DEPS_DIR)/lib64 -lboost_filesystem-gcc-mt-1_33
   DSO_PLUGIN_DEPS+= -L$(VJ_DEPS_DIR)/lib64 -lboost_filesystem-gcc-mt-1_33
   else
   EXTRA_LIBS+= -L$(VJ_DEPS_DIR)/lib -lboost_filesystem-gcc-mt-1_33
   DSO_PLUGIN_DEPS+= -L$(VJ_DEPS_DIR)/lib -lboost_filesystem-gcc-mt-1_33
   endif
endif
endif
EXTRA_INCLUDES+= -I$(VJ_DEPS_DIR)/include
