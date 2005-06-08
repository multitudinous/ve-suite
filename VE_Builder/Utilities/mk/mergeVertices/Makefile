# Create the OS-dependent directory name
APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/mergeVertices

all:
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)
#	-$(MAKE) install

srcdir= ../../

SRCS=\
   mergeVertices.cpp \
   fileIO.cxx \
   cfdVTKFileHandler.cxx \
   readWriteVtkThings.cxx

# Specify the OS-dependent directory name for object files and depends
OBJDIR=../../$(CFDHOSTTYPE)
DEPDIR=../../$(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

EXTRA_CFLAGS+=  

EXTRA_CXXFLAGS+=

EXTRA_DEFS+=

EXTRA_INCLUDES+= -I${VE_SUITE_HOME}/VE_Xplorer \
                 -I$(VE_SUITE_HOME)/VE_Builder/Translator

#EXTRA_DEBUG_FLAGS+= -g -D_DEBUG
#EXTRA_OPTIM_FLAGS+= -O2 -OPT:Olimit=0 -D_OPT

LINKALL_ON= -all
LINKALL_OFF= 

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

EXTRA_LIBS+=

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk

#add vtk stuff to EXTRA_INCLUDES and EXTRA_LIBS
include $(VE_SUITE_HOME)/VE_Installer/dzr.vtk.mk

vpath %.cxx $(VE_SUITE_HOME)/VE_Xplorer $(VE_SUITE_HOME)/VE_Builder/Translator
vpath %.cpp $(VE_SUITE_HOME)/VE_Builder/Translator
#vpath %.h   $(VE_SUITE_HOME)/VE_Xplorer $(VE_SUITE_HOME)/VE_Builder/Translator

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

#install:
#	cp NetworkTestBean.jar $(TWEEK_BASE_DIR)/bin/beans
#	cp $(srcdir)/NetworkTestBean.xml $(TWEEK_BASE_DIR)/bin/beans

CLEAN_FILES+= $(APP_NAME)
