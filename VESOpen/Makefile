#__________________________________________________________
# MAKEFILE
#__________________________________________________________

default: all

all:
	$(MAKE) cxx_idl

srcdir= .

# turn off dependencies for idl stuff, make clean
NO_DEPEND= YES

CONDUCTOR_BASE_DIR= ${VE_SUITE_HOME}/VE_Conductor

# list the cpp and h files here
IDL_CXX_FILES= module.h \
               module.cpp \
               VjObs.h \
               VjObs.cpp

# list the corresponding java files that are in directory "networktest"
# These files correspond to the IDL_CXX_FILES listed above

include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

CXX_GEN_DIR= $(srcdir)/skel
CXX_IDL=      tao_idl
CXX_IDL_OPTS=  -o $(CXX_GEN_DIR) -GI -I./idl
CXX_IDL_INCLUDES= 

vpath %.idl $(srcdir)/idl $(srcdir)

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx_idl: $(IDL_CXX_FILES)

%.cpp %.h: %.idl
	$(CXX_IDL) $(CXX_IDL_OPTS) $(CXX_IDL_INCLUDES) $<

# in addition to the created *.o files and any core files, the following files will be removed during "gmake clean"
CLEAN_FILES+= $(CXX_GEN_DIR)/* 
