#APP_NAME=

all:
	$(MAKE) NO_DEPEND=0 cxx
#	$(MAKE) $(APP_NAME)

srcdir=.

SRCS= VTKtoOSG.cpp 

OBJDIR=$(CFDHOSTTYPE)
DEPDIR=$(CFDHOSTTYPE)
NO_DEPEND= YES

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

EXTRA_CFLAGS+=
EXTRA_CXXFLAGS+=
EXTRA_DEFS+=
#EXTRA_INCLUDES+= -I/usr/local/include

#EXTRA_DEBUG_FLAGS+= -g -D_DEBUG
#EXTRA_OPTIM_FLAGS+= -O2 -OPT:Olimit=0 -D_OPT

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

EXTRA_LIBS+=

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk
include $(VE_SUITE_HOME)/VE_Installer/dzr.vtk.mk

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

#${APP_NAME}: $(OBJS)
#	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

CLEAN_FILES+= $(APP_NAME)
