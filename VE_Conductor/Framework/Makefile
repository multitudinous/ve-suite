# Create the OS-dependent directory name
CFDHOSTTYPE?=	$(DZR_HOSTTYPE)
VE_SUITE_HOME?= ../../
BUNDLE=$(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/WinClient.app
APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/WinClient.app/Contents/MacOS/WinClient
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
   DataSetLoaderUI.cxx\
   Frame.cpp \
   Network.cpp \
   NavigationPane.cxx \
   OrbThread.cpp \
   paraThread.cpp \
   PluginLoader.cpp \
   StreamersPane.cxx \
   UI_i.cpp \
   UI_TransientDialog.cxx \
   SoundsPane.cpp \
   ViewLocPane.cxx\
   TBToolBar.cxx \
   TransformUI.cxx\
   vistab.cpp\
   vectors.cpp\
   advancedvectors.cpp\
   contours.cpp\
   advancedcontours.cpp\
   streamlines.cpp\
   advancedstreamlines.cpp\
   isosurfaces.cpp\
   CORBAServiceList.cxx

# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

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
EXTRA_LDFLAGS+= -Wl,-E -g #-ldl -lrt -lc 
endif
EXTRA_LIBS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) -lVE_ConductorLib
EXTRA_LIBS+= -lGUIPluginLib -lVE_XMLLib -lm -lpthread 
EXTRA_LIBS+= -lVE_ModelLib 
EXTRA_LIBS+= -lVE_CADLib 
EXTRA_LIBS+= -lVE_ShaderLib 
EXTRA_LIBS+= -lVE_Conductor_UtilitiesLib
EXTRA_LIBS+= -lVE_OpenModuleLib

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
                        $(VE_SUITE_HOME)/VE_Builder/Utilities/gui \
                        $(VE_SUITE_HOME)/VE_Conductor/Utilities

include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk

include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.wx.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ace_tao.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.xerces.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.vtk.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.boost.mk


EXTRA_CXXFLAGS+= -g
EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)

ICONFILE=$(VE_SUITE_HOME)/VE_Conductor/wxmac.icns
INFO_FILE=$(VE_SUITE_HOME)/VE_Conductor/Info.plist
BUNDLESIGNATURE=????
# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
${APP_NAME}: $(OBJS)
	mkdir -p $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)
ifeq ($(CFDHOSTTYPE), Darwin)
#	mkdir -p $(BUNDLE)
#	mkdir -p $(BUNDLE)/Contents 
#	mkdir -p $(BUNDLE)/Contents/MacOS 
#	mkdir -p $(BUNDLE)/Contents/Resources 
	$(LINK) $(LINK_OUT)$@ $(OBJS)
	$(shell wx-config --rezflags ) $(APP_NAME)
#	/Developer/Tools/SetFile -a C $(APP_NAME)
#	echo -n "APPL$(BUNDLESIGNATURE)" > $(BUNDLE)/Contents/PkgInfo
#	cp -f $(ICONFILE) $(BUNDLE)/Contents/Resources/wxmac.icns
#	cp -f $(INFO_FILE) $(BUNDLE)/Contents/Info.plist
else
	$(LINK) $(LINK_OUT)$@ $(OBJS)
endif
     
#install:
#	cp NetworkTestBean.jar $(TWEEK_BASE_DIR)/bin/beans
#	cp $(srcdir)/NetworkTestBean.xml $(TWEEK_BASE_DIR)/bin/beans

CLEAN_FILES+= $(APP_NAME)
