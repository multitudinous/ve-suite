CFDHOSTTYPE?=	$(DZR_HOSTTYPE)

APP_NAME= ../Units/$(CFDHOSTTYPE)/HyperLabUnitApp

srcdir= .

all:
	mkdir -p ../Units/$(CFDHOSTTYPE)
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

SRCS=\
   HyperLabUnit_i.cpp \
   HyperLabUnit_client.cpp

# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
#DEBUG_APP?= TRUE
OPTIM_APP?= TRUE

EXTRA_CFLAGS+=

EXTRA_CXXFLAGS+=

EXTRA_DEFS+=

EXTRA_LIBS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) 
EXTRA_LIBS+= -lVE_ConductorLib
EXTRA_LIBS+= -lVE_OpenModuleLib

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

#EXTRA_PATH_FOR_SOURCES+= $(VE_SUITE_HOME)/VEOpen/skel 

EXTRA_INCLUDES += -I$(VE_SUITE_HOME)
EXTRA_INCLUDES += -I$(VE_SUITE_HOME)/VE_Conductor/Network

include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.apr.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ace_tao.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.xerces.mk

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

CLEAN_FILES+= $(APP_NAME)

