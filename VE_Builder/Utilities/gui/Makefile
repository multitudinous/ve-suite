# Create the OS-dependent directory name
APP_NAME= toolBarApp
WX_CXX = $(shell wx-config --cxxflags)
WX_LIB = $(shell wx-config --libs)

all:
	$(MAKE) NO_DEPEND=0 cxx 
	$(MAKE) $(APP_NAME)
#	-$(MAKE) install

srcdir= .


SRCS=\
toolBarApp.cpp \
readWriteVtkThings.cxx\
cfdGrid2Surface.cpp\
setScalarAndVector.cpp
# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

EXTRA_CFLAGS+= 

EXTRA_CXXFLAGS+=${WX_CXX} -DVTK_USE_ANSI_STDLIB

#EXTRA_DEFS+= -DSJK_TEST

EXTRA_INCLUDES+= -I${VE_SUITE_HOME}/VE_Xplorer -I${VE_SUITE_HOME}/VE_Builder/Translator -I${VE_SUITE_HOME}/VE_Builder/Utilities

#EXTRA_DEBUG_FLAGS+= -g -D_DEBUG
#EXTRA_OPTIM_FLAGS+= -O2 -OPT:Olimit=0 -D_OPT

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

EXTRA_LIBS+= -lm ${WX_LIB} /lib/libuuid.so.1

#vrj.appdefs.mk includes $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk

#add vtk stuff to EXTRA_INCLUDES and EXTRA_LIBS
include $(VE_SUITE_HOME)/VE_Installer/dzr.vtk.mk

vpath %.cxx $(VE_SUITE_HOME)/VE_Xplorer 
vpath %.h    $(VE_SUITE_HOME)/VE_Xplorer 
vpath %.cpp $(VE_SUITE_HOME)/VE_Builder/Translator
vpath %.h    $(VE_SUITE_HOME)/VE_Builder/Translator
vpath %.cpp $(VE_SUITE_HOME)/VE_Builder/Utilities
vpath %.h    $(VE_SUITE_HOME)/VE_Builder/Utilities

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
${APP_NAME}: $(OBJS)
	mkdir -p $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

#install:
#	cp NetworkTestBean.jar $(TWEEK_BASE_DIR)/bin/beans
#	cp $(srcdir)/NetworkTestBean.xml $(TWEEK_BASE_DIR)/bin/beans

CLEAN_FILES+= $(APP_NAME)
