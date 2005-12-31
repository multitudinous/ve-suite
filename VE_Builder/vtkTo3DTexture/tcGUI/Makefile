# Create the OS-dependent directory name
MPI_APP = FALSE
ifeq ($(MPI_APP), TRUE)
   APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/vtkTo3DTextureMPI
else
   APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/vtkTo3DTexture
endif

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
ifeq ($(MPI_APP), TRUE)
   OBJDIR= $(CFDHOSTTYPE)_MPI
   DEPDIR= $(CFDHOSTTYPE)_MPI
else
   OBJDIR= $(CFDHOSTTYPE)
   DEPDIR= $(CFDHOSTTYPE)
endif

NO_DEPEND= YES

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

EXTRA_CXXFLAGS+= -I$(VJ_DEPS_DIR)/include
EXTRA_LIBS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) -lVE_UtilLib
ifeq ($(MPI_APP), TRUE)
   EXTRA_LIBS+= -lmpi
   EXTRA_CXXFLAGS+= -DUSE_MPI
   CXX = mpiCC
endif

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk

#add vtk stuff to EXTRA_INCLUDES and EXTRA_LIBS
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.wx.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.vtk.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.boost.mk

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

CLEAN_FILES+= $(APP_NAME)
