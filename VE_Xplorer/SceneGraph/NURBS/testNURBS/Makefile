# Create the OS-dependent directory name
APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/testNURBS

all:
#	cd $(VE_SUITE_HOME)/VE_Xplorer/VE_Util && $(MAKE)
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

srcdir= .

SRCS=\
	NURBTest.cxx 
# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

EXTRA_CFLAGS+= 

#for some debug print information in the ansys translator...
#EXTRA_CXXFLAGS+= -DPRINT_HEADERS

#EXTRA_DEFS+= -DSJK_TEST

EXTRA_INCLUDES+= -I${VE_SUITE_HOME}

#EXTRA_DEBUG_FLAGS+= -g -D_DEBUG
#EXTRA_OPTIM_FLAGS+= -O2 -OPT:Olimit=0 -D_OPT

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

EXTRA_LIBS+= -lm -L$(VJ_DEPS_DIR)/lib
EXTRA_LIBS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) -lVE_NURBSLib 
EXTRA_PATH_FOR_SOURCES+= $(VE_SUITE_HOME)/VE_Builder/Translator/DataLoader
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.boost.mk
#vrj.appdefs.mk 
include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
#include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk

#add vtk stuff to EXTRA_INCLUDES and EXTRA_LIBS
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.osg.mk


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
