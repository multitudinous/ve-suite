# Create the OS-dependent directory name
APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/HDFToVtk

all:
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)
#	-$(MAKE) install

srcdir= .

SRCS=\
   cfdHDFtoVTK.cpp \
   translatorTest.cpp \
   viewCells.cpp 

# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

EXTRA_CFLAGS+= 

#for some debug print information in the ansys translator...
#EXTRA_CXXFLAGS+= -DPRINT_HEADERS

#EXTRA_DEFS+= -DSJK_TEST

EXTRA_INCLUDES+= -I${HDF4_HOME}/include -I${HDF5_HOME}/include

#EXTRA_DEBUG_FLAGS+= -g -D_DEBUG
#EXTRA_OPTIM_FLAGS+= -O2 -OPT:Olimit=0 -D_OPT

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@
EXTRA_LIB_DIRS+= -L${HDF4_HOME}/lib -L${HDF5_HOME}/lib
EXTRA_LIBS+= -L${HDF4_HOME}/lib -L${HDF5_HOME}/lib -lm -ldf -lmfhdf -lhdf5

#vrj.appdefs.mk includes $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk

#add vtk stuff to EXTRA_INCLUDES and EXTRA_LIBS
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.vtk.mk

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
