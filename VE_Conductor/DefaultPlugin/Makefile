# Create the OS-dependent directory name
CFDHOSTTYPE?=	$(DZR_HOSTTYPE)
PLUGIN = HUMMER
APP_NAME= $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE)/lib$(PLUGIN).so

VE_CONDUCTOR_HOME = $(VE_SUITE_HOME)/VE_Conductor

all:
	mkdir -p ../GUIPlugins/$(CFDHOSTTYPE)
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

srcdir= .

SRCS=\
   DefaultPlugin.cpp \
	DefaultPlugin_UI_Dialog.cpp 

# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
#DEBUG_APP?= TRUE
OPTIM_APP?= TRUE

EXTRA_CFLAGS+=

EXTRA_CXXFLAGS+= 

EXTRA_INCLUDES+= -I$(VE_CONDUCTOR_HOME)/Framework 
EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)

EXTRA_DEFS+=

EXTRA_LIBS+= -shared 
# IRIX needs these flags
CFDUNAME = $(shell uname)
ifeq (${CFDUNAME},IRIX64)
   EXTRA_CXXFLAGS+= 
   EXTRA_LIBS+= 
else
   EXTRA_CXXFLAGS+= -fPIC
   EXTRA_LIBS+= -fPIC
endif

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk

include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.wx.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.xerces.mk

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

CLEAN_FILES+= $(APP_NAME)
