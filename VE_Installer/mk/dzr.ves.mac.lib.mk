default: plugin-dso

# Don't require that the user has $GADGET_BASE_DIR set as an environment
# variable.  We'll assume that if $VJ_BASE_DIR is set, it points to the place
# where Gadgeteer is installed as well.
GADGET_BASE_DIR?=	$(VJ_BASE_DIR)
DZR_BASE_DIR?=		$(GADGET_BASE_DIR)/share/Doozer

# -----------------------------------------------------------------------------
# Base variables for compiling and linking.
# -----------------------------------------------------------------------------
CC=			gcc
CXX=			g++
CFLAGS_DYNLIB=		-fPIC -DPIC
CC_PROF_FLAGS=		-pg
CXXFLAGS_DYNLIB=	-fPIC -DPIC
CXX_PROF_FLAGS= 	-pg
C_DLL=			$(CC) -dynamiclib $(DPP_EXTRA_LDFLAGS) $(EXTRA_LDFLAGS)
CXX_DLL=		$(CXX) -dynamiclib $(DPP_EXTRA_LDFLAGS) $(EXTRA_LDFLAGS)
C_PLUGIN=		$(CC) -bundle $(DPP_EXTRA_LDFLAGS) $(EXTRA_LDFLAGS)
CXX_PLUGIN=		$(CXX) -bundle $(DPP_EXTRA_LDFLAGS) $(EXTRA_LDFLAGS)
DEP_GEN_FLAG=		-M
DYLIB_NAME_FLAG=	-o $@
JDK_HOME=		@JDK_HOME@
LD=			$(CXX) -dynamiclib $(DPP_EXTRA_LDFLAGS) $(EXTRA_LDFLAGS)
LDOPTS=			 $(DPP_EXTRA_LDOPTS) $(EXTRA_LDOPTS)	\
			$(DSOVERSIONOPTS)
LDOPTS_DBG=		 $(EXTRA_LDOPTS_DBG)
LDOPTS_OPT=		 $(EXTRA_LDOPTS_OPT)
LDOPTS_PROF=		 -pg $(EXTRA_LDOPTS_PROF)
OBJDIR?=		.
PROFLIB_EXT=		_p
PROF_OPT_FLAGS=		$(DBG_FLAGS)

# -----------------------------------------------------------------------------
# Plug-in related settings.
# -----------------------------------------------------------------------------
DYNAMICLIB_EXT=         dylib
OBJEXT=			o
OBJ_BUILD_FLAG=		-c
OBJ_NAME_FLAG=		-o $@
OS_TYPE=		UNIX

# SGI-specific dynamic-shared-object information.
DSOREGFILE=		
DSOVERSION=		
DSOVERSIONOPTS=		

# -----------------------------------------------------------------------------
# Extensions to the base Doozer compiler and linker flags that are required
# for building Gadgeteer device plugin plug-ins.
# -----------------------------------------------------------------------------
EXTRA_CFLAGS+=		-fno-common  -pipe $(OPTIMIZER) $(CFLAGS_DYNLIB) $(EXTRA_DEFS)
EXTRA_CXXFLAGS+=	-fno-common  -pipe -Wno-long-double $(OPTIMIZER) $(CXXFLAGS_DYNLIB) $(EXTRA_DEFS)
EXTRA_DEFS= 
EXTRA_DEPENDFLAGS+= $(EXTRA_INCLUDES)
EXTRA_INCLUDES+=	
EXTRA_LDFLAGS+=		 
EXTRA_LIBS+=		

# -----------------------------------------------------------------------------
# Helper utilities.
# -----------------------------------------------------------------------------
LN_S=			ln -s
MAKEDEPEND=		
PERL=			/usr/bin/perl
RANLIB=			ranlib
RM_LN=			rm -f

# -----------------------------------------------------------------------------
# Common code for plugin DSOs.
# -----------------------------------------------------------------------------
ifeq ($(BUILD_TYPE), dbg)
   BUILD_TYPE_EXT=	_d
   DEBUG_APP=		TRUE
   OPTIM_APP=		FALSE
   EXTRA_DEFS+=		-D_DEBUG
else
   BUILD_TYPE_EXT=	
   DEBUG_APP=		FALSE
   OPTIM_APP=		TRUE
   EXTRA_DEFS+=		-D_OPT -DNDEBUG
endif

#DSO_PLUGIN_DEPS=	$(shell gadgeteer-config --libs)		\
			$(shell gadgeteer-config --extra-libs)

PLUGIN_DSO=	$(PLUGIN_NAME)$(BUILD_TYPE_EXT).$(DYNAMICLIB_EXT)
DYLIB_DEPS=	$(PRE_DSO_PLUGIN_DEPS) $(DSO_PLUGIN_DEPS)		\
		$(POST_DSO_PLUGIN_DEPS)

# It is critical that this be included before the $(PLUGIN_DSO) target below.
include $(DZR_BASE_DIR)/mk/dzr.lib.mk
plugin-dso: $(PLUGIN_DSO)

$(PLUGIN_DSO): $(OBJS)
	$(CXX_DLL) $(LDOPTS) $(DYLIB_NAME_FLAG) $(OBJS) $(DYLIB_DEPS)

CLOBBER_FILES+=	$(PLUGIN_DSO)
