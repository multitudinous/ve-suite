# this Makefile requires gmake

# store some wxWidgets information
CXX = $(shell wx-config --cxx)
WX_CXX = $(shell wx-config --cxxflags)
WX_LIB = $(shell wx-config --libs)

# Create the OS-dependent directory name
UNAME= $(shell uname -s)
LIBS_DIR= $(UNAME)_LIB

APP_NAME= cfdtrans

all:
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

#add vtk stuff to EXTRA_INCLUDES and EXTRA_LIBS
include $(VE_SUITE_HOME)/VE_Installer/dzr.vtk.mk

srcdir=		.

SRCS=\
   converter.cpp \
   FormatSelector.cpp \
   mFixTranslator.cpp \
   FluentTranslator.cpp \
   UnstructuredGridWriter.cpp

OBJDIR=	$(LIBS_DIR)
DEPDIR=	$(LIBS_DIR)
NO_DEPEND= YES
# One of these must be uncommented.
#DEBUG_APP?=		TRUE
OPTIM_APP?=		TRUE

EXTRA_CFLAGS+=		 
EXTRA_CXXFLAGS+= ${WX_CXX}
EXTRA_DEFS+=
#EXTRA_DEBUG_FLAGS+=	-g -D_DEBUG
#EXTRA_OPTIM_FLAGS+=	-O2 -OPT:Olimit=0 -D_OPT

LINKALL_ON=		-all
LINKALL_OFF=		

STATIC_ON=		@APP_LINK_STATIC_ON@
STATIC_OFF=		@APP_LINK_STATIC_OFF@

include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
include $(VE_SUITE_HOME)/VE_Installer/dzr.vtk.mk
EXTRA_LIBS+= -lm ${WX_LIB}

help:
	@echo "The valid targets for this Makefile:"
	@echo "... all (the default if no target is provided)"
	@echo "... clean"
	@echo "... cleandepend"
	@echo ""

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

CLEAN_FILES+= $(APP_NAME)
