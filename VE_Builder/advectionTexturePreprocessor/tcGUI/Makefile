# Create the OS-dependent directory name
APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/advectionTexturePreprocessor

all:
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)
#	-$(MAKE) install

srcdir= .

SRCS=\
   flowTexture.cpp \
   tcApp.cpp \
   tcFrame.cpp \
   textureCreator.cpp

# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

EXTRA_CFLAGS+= 


LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

#vrj.appdefs.mk includes $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk

#add vtk stuff to EXTRA_INCLUDES and EXTRA_LIBS
include $(VE_SUITE_HOME)/VE_Installer/dzr.wx.mk
include $(VE_SUITE_HOME)/VE_Installer/dzr.vtk.mk

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

CLEAN_FILES+= $(APP_NAME)
