# Create the OS-dependent directory name
CFDHOSTTYPE?=	$(DZR_HOSTTYPE)
VE_SUITE_HOME?= ../

APP_NAME= WebServices

IDL_HOME = $(VE_SUITE_HOME)/VEOpen

srcdir= ./src/

all:
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

SRCS=\
   cfdWebServicesMain.cxx \
   cfdWebServices.cxx \
   interface.cpp \
   moduleC.cpp \
   moduleS.cpp \
   Network_Exec.cpp \
   package.cpp \
   string_ops.cpp \
   veWebService_i.cpp \
   VjObsS.cpp \
   VjObsC.cpp
   

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

EXTRA_LIBS+=

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

include $(VE_SUITE_HOME)/VE_Installer/dzr.wx.mk
include $(VE_SUITE_HOME)/VE_Installer/dzr.ace_tao.mk
include $(VE_SUITE_HOME)/VE_Installer/dzr.xerces.mk

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk
#include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
EXEC_TAG = _tao
vpath %.cpp $(VE_SUITE_HOME)/VE_Conductor/Framework \
			$(VE_SUITE_HOME)/VE_Xplorer \
			$(VE_SUITE_HOME)/VE_Conductor/VE_UI $(IDL_HOME)/skel \
			$(VE_SUITE_HOME)/VE_SceneGraph \
			$(VE_SUITE_HOME)/VE_CE			\

EXTRA_CXXFLAGS += -I$(VE_SUITE_HOME)/VE_Conductor/Framework -I$(IDL_HOME)/skel 
EXTRA_CXXFLAGS += -I$(VE_SUITE_HOME)/VE_Conductor/VE_UI 
EXTRA_CXXFLAGS += -I$(VJ_BASE_DIR)/include -I$(VE_SUITE_HOME)/VE_CE -I$(VE_SUITE_HOME)/VE_Xplorer -I$(VE_SUITE_HOME)/VE_SceneGraph 
#EXTRA_CXXFLAGS += -I$(VJ_DEPS_DIR)/include 
#EXTRA_CXXFLAGS += -I$(WX_HOME)/include/
#EXTRA_CXXFLAGS += -I$(WX_HOME)/lib/wx/include/gtkd-2.4
#EXTRA_CXXFLAGS += -I$(VJ_DEPS_DIR)/include/boost/compatibility/cpp_c_headers
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
