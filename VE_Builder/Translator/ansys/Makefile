APP_NAME= ansysReader

all:
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

srcdir= .

SRCS= ansysReader.cpp fileIO.cxx converter.cpp \
      test.cpp readWriteVtkThings.cpp

# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

EXTRA_INCLUDES+= -I${VE_SUITE_HOME}/VE_Xplorer \
                 -I${VE_SUITE_HOME}/VE_Builder/Translator

LINKALL_ON= -all
LINKALL_OFF=

#for some debug print information...
EXTRA_CXXFLAGS+= -DPRINT_HEADERS

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

EXTRA_LIBS+= -lm

#vrj.appdefs.mk includes $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk

#add vtk stuff to EXTRA_INCLUDES and EXTRA_LIBS
include $(VE_SUITE_HOME)/VE_Installer/dzr.vtk.mk

vpath %.cxx $(VE_SUITE_HOME)/VE_Xplorer
vpath %.cpp ${VE_SUITE_HOME}/VE_Builder/Translator

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

CLEAN_FILES+= $(APP_NAME)
