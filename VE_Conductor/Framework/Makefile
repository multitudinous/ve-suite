# Create the OS-dependent directory name
CFDHOSTTYPE?=	$(DZR_HOSTTYPE)
VE_SUITE_HOME?= ../../

APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/WinClient

VE_UI_HOME = ${VE_SUITE_HOME}/VE_Conductor/VE_UI
IDL_HOME = ${VE_SUITE_HOME}/VE_Open

all:
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

srcdir= .

SRCS=\
   App.cpp \
   Avail_Modules.cpp \
   Frame.cpp \
   moduleC.cpp \
   moduleS.cpp \
   Network.cpp \
   NavigationPane.cxx \
   OrbThread.cpp \
   paraThread.cpp \
   PluginLoader.cpp \
   VjObsC.cpp \
   VjObsS.cpp \
   UI_AdvectionPanel.cxx \
   UI_i.cpp \
   UI_Frame.cpp \
   UI_Tabs.cpp \
   UI_VisTab.cpp \
   UI_VecTab.cpp \
   UI_StreamTab.cpp \
   UI_DataSetPanel.cpp \
   UI_GeometryTab.cpp \
   UI_TeacherTab.cpp \
   UI_TransTab.cpp \
   UI_TransientDialog.cxx \
   UI_VertTab.cpp \
   UI_DesignParTab.cpp \
   UI_ViewLocTab.cpp \
   UI_ModSelPanel.cpp \
   UI_ModelData.cpp \
   spinctld.cpp\
   SoundsPane.cpp 

# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
#DEBUG_APP?= TRUE
OPTIM_APP?= TRUE

EXTRA_CFLAGS+=

#EXTRA_CXXFLAGS+= -DAPP_NO_THREADS -DXML_USE_NO_THREADS -D_TAO -w
EXTRA_CXXFLAGS+= -D_TAO 

ifeq ($(VE_PATENTED), TRUE)
   EXTRA_CXXFLAGS+= -DVE_PATENTED
endif

EXTRA_DEFS+=

ifeq (${CFDHOSTTYPE},Darwin)
#EXTRA_LDFLAGS+= -Wl
else
EXTRA_LDFLAGS+= -Wl,-E #-ldl -lrt -lc 
endif
EXTRA_LIBS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) -lVE_ConductorLib
EXTRA_LIBS+= -lGUIPluginLib -lVE_XMLLib -lm -lpthread 
EXTRA_LIBS+= -lVE_ModelLib 
EXTRA_LIBS+= -lVE_CADLib 
EXTRA_LIBS+= -lVE_ShaderLib 

ifeq (${CFDHOSTTYPE},IRIX64)
   # For ANSI std headers
   EXTRA_INCLUDES+= -I$(VJ_DEPS_DIR)/include/boost/compatibility/cpp_c_headers
   EXTRA_LIBS+= -L/usr/freeware/lib32 -lm -lpthread 
endif

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

EXTRA_PATH_FOR_SOURCES+= $(VE_UI_HOME) $(IDL_HOME)/skel \
                        $(VE_SUITE_HOME)/VE_Conductor/Framework \
                        $(VE_SUITE_HOME)/VE_Builder/Utilities/gui
include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk

include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.wx.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ace_tao.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.xerces.mk

EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
${APP_NAME}: $(OBJS)
	mkdir -p $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)
ifeq ($(CFDHOSTTYPE), Darwin)
	$(shell wx-config --rezflags ) $(APP_NAME)
endif

#install:
#	cp NetworkTestBean.jar $(TWEEK_BASE_DIR)/bin/beans
#	cp $(srcdir)/NetworkTestBean.xml $(TWEEK_BASE_DIR)/bin/beans

CLEAN_FILES+= $(APP_NAME)
