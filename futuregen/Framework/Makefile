# Create the OS-dependent directory name
CFDHOSTTYPE?=	$(DZR_HOSTTYPE)
VE_SUITE_HOME?= ../../

APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/WinClient

VE_UI_HOME = ${VE_SUITE_HOME}/futuregen/VE_UI
IDL_HOME = ${VE_SUITE_HOME}/futuregen/IDL
SKEL_HOME = ${VE_SUITE_HOME}/VE_Conductor/skel

all:
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)
#	-$(MAKE) install

srcdir= .

SRCS=\
   moduleC.cpp \
   moduleS.cpp \
   StringParse.cpp \
   OrbThread.cpp \
   Frame.cpp \
   VjObsC.cpp \
   VjObsS.cpp \
   App.cpp \
   PluginLoader.cpp \
   Network.cpp \
   Avail_Modules.cpp \
   ListTable.cpp \
   PortDialog.cpp \
   ResultPanel.cpp \
   paraThread.cpp \
   GlobalParamDialog.cpp \
   string_ops.cpp \
   interface.cpp \
   package.cpp \
   Plugin_base.cpp \
   UIDialog.cpp \
   TexTable.cpp \
   TextResultDialog.cpp \
   FinancialDialog.cpp \
   UI_i.cpp \
   UI_Frame.cpp \
   UI_Tabs.cpp \
   UI_VisTab.cpp \
   UI_VecTab.cpp \
   UI_StreamTab.cpp \
   UI_DataSetPanel.cpp \
   UI_SoundsTab.cpp \
   UI_GeometryTab.cpp \
   UI_TeacherTab.cpp \
   UI_NavTab.cpp  \
   UI_TransTab.cpp \
   UI_VertTab.cpp \
   UI_DesignParTab.cpp \
   UI_ViewLocTab.cpp \
   UI_ModSelPanel.cpp \
   UI_ModelData.cpp

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

EXTRA_LIBS+= -lm

ifeq ($(CFDHOSTTYPE),IRIX64)
   EXTRA_INCLUDES+= -I$(VJ_DEPS_DIR)/include/boost/compatibility/cpp_c_headers
endif

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk

include $(VE_SUITE_HOME)/VE_Installer/dzr.wx.mk
include $(VE_SUITE_HOME)/VE_Installer/dzr.ace_tao.mk
include $(VE_SUITE_HOME)/VE_Installer/dzr.xerces.mk

vpath %.cpp ${VE_UI_HOME} ${IDL_HOME} 
vpath %.cpp ${SKEL_HOME} ${VE_SUITE_HOME}/futuregen/Plugin

EXTRA_CXXFLAGS += -I${VE_SUITE_HOME}/futuregen/Plugin -I${VE_UI_HOME} \
                  -I${IDL_HOME} -I${VE_SUITE_HOME}/VE_Xplorer -I${SKEL_HOME}
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
