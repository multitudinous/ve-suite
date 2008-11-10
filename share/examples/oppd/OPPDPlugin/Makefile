# Create the OS-dependent directory name
CFDHOSTTYPE?=	$(DZR_HOSTTYPE)
PLUGIN = OPPD
APP_NAME= ../GUIPlugins/$(CFDHOSTTYPE)/lib$(PLUGIN).so

all:
	mkdir -p ../GUIPlugins/$(CFDHOSTTYPE)
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

srcdir= .

SRCS=\
   OPPD.cpp \
	OPPD_UI.cpp \
   OPPD_UI_entries.cpp \
   OPPD_UI_outputs.cpp \
   OPPD_UI_tabs.cpp \
   OPPD_UI_constants.cpp 

# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
#DEBUG_APP?= TRUE
OPTIM_APP?= TRUE

EXTRA_CFLAGS+=

EXTRA_CXXFLAGS+= 

EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)/VE_Conductor/Framework 
EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)

EXTRA_DEFS+=

EXTRA_LIBS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) -lVE_ConductorLib
EXTRA_LIBS+= -lGUIPluginLib

EXTRA_LDFLAGS+= -shared 

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

