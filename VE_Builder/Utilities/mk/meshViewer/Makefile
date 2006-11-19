# Create the OS-dependent directory name
APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/meshViewer

ifeq ($(SCENE_GRAPH), OSG)
   OSG = _osg
else
   OSG = _pf
endif

ifeq ($(TAO_BUILD), TRUE)
   EXEC_TAG = _tao
endif

# for osg and patented stuff
ifeq ($(VE_PATENTED), TRUE)
   PATENTED_TAG = _vep
endif

all:
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)
#	-$(MAKE) install

srcdir= ../../

SRCS=\
   meshViewer.cpp \
   setScalarAndVector.cpp \
   viewCells.cpp 

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

EXTRA_INCLUDES+= -I${VE_SUITE_HOME}

#EXTRA_DEBUG_FLAGS+= -g -D_DEBUG
#EXTRA_OPTIM_FLAGS+= -O2 -OPT:Olimit=0 -D_OPT

LINKALL_ON= -all
LINKALL_OFF= 

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

EXTRA_LIBS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) 
EXTRA_LIBS+= -lVE_UtilLib

EXTRA_PATH_FOR_SOURCES+=$(VE_SUITE_HOME)/VE_Builder/Translator

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk
#add vtk stuff to EXTRA_INCLUDES and EXTRA_LIBS
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.vtk.mk


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
