#_______________________________________________________________________________
# MAKEFILE
#_______________________________________________________________________________
#
#_______________________________CUSTOM OPTIONS__________________________________
#
# Uncomment the following line to use CFDCOMMANDARRAY.
#EXTRA_CXXFLAGS+= -D_CFDCOMMANDARRAY
#
# Uncomment this line if you want to build a cluster juggler app
#CLUSTER_APP = TRUE
#_______________________________________________________________________________

CONDUCTOR_BASE_DIR= ${VE_SUITE_HOME}/VE_Conductor

# If OMNI_HOME was defined in the script setup.tsh, 
# then define CORBA_DEFINES, and CORBA_INCLUDES.
# (To make a non-corba version, don't define OMNI_HOME.)
ifdef OMNI_HOME
   vpath %.cxx    ${CONDUCTOR_BASE_DIR}/impl

   CORBA_DEFINES = -D__OSVERSION__=6 

   ifeq (`uname`, Linux)
      CORBA_DEFINES += -D__x86__ -D__linux__
      OMNI_LIB = ${OMNI_HOME}/lib
   else
      CORBA_DEFINES += -D__mips__ -D__irix__
      OMNI_LIB = ${OMNI_HOME}/lib
   endif

   OMNILIB = -L${OMNI_LIB} -lomniORB4 -lomnithread -lomniDynamic4
   
   CORBA_INCLUDES = -I${OMNI_HOME}/include 
   EXTRA_INCLUDES+=	-I${CONDUCTOR_BASE_DIR}/impl \
                     -I${CONDUCTOR_BASE_DIR}/skel \
                     ${CORBA_INCLUDES}
   EXEC_TAG =
endif

ifdef ACE_ROOT
   FUTUREGEN_HOME = /home/users/mccdo/REI/futuregen

   vpath %.cxx    ${CONDUCTOR_BASE_DIR}/impl 
   vpath %.cpp    ${FUTUREGEN_HOME}/IDL.old ${FUTUREGEN_HOME}/Plugin
   #----------------------------------------------------------------------------
   # Include macros and targets
   #----------------------------------------------------------------------------

   include ${ACE_ROOT}/include/makeinclude/wrapper_macros.GNU
   include ${ACE_ROOT}/include/makeinclude/macros.GNU
#   include ${TAO_ROOT}/rules.tao.GNU
   include ${ACE_ROOT}/include/makeinclude/rules.common.GNU
   include ${ACE_ROOT}/include/makeinclude/rules.nonested.GNU
   include ${ACE_ROOT}/include/makeinclude/rules.local.GNU
   include ${TAO_ROOT}/taoconfig.mk

   ACE_LIBS = -L${ACE_ROOT}/ace -lACE -lTAO_CosNaming -lTAO_Svc_Utils -lTAO_IORTable -lTAO_PortableServer -L${TAO_ROOT}/tao -lTAO 
#${LDFLAGS}
   ACE_INCLUDES = -I${TAO_ROOT}/orbsvcs
   EXTRA_INCLUDES+=	\
                  ${ACE_INCLUDES} \
                  -I${CONDUCTOR_BASE_DIR}/impl \
                  -I${CONDUCTOR_BASE_DIR}/skel \
                  -I${FUTUREGEN_HOME}/IDL.old \
                  -I${FUTUREGEN_HOME}/Plugin
   EXEC_TAG = _tao
endif

#__________________________________________________________
# list the source files that are in current directory
SRCS=\
   Plot3Dviewer.cxx \
   cfdAnimatedStreamlineCone.cxx \
   cfdAnimatedImage.cxx \
   cfdAnimation.cxx \
   cfdApp.cxx \
   cfdContour.cxx \
   cfdContourBase.cxx \
   cfdContours.cxx \
   cfdCursor.cxx \
   cfdCuttingPlane.cxx \
   cfdDataSet.cxx \
   cfdFILE.cxx \
   cfdFileInfo.cxx \
   cfdFrame.cxx \
   cfdGeode.cxx \
   cfdImage.cxx \
   cfdIsosurface.cxx \
   cfdLaser.cxx \
   cfdMenu.cxx \
   cfdMomentum.cxx \
   cfdMomentums.cxx \
   cfdNavigate.cxx \
   cfdNodeTraverser.cxx \
   cfdObjects.cxx \
   cfdPlanes.cxx \
   cfdPresetContour.cxx \
   cfdPresetMomentum.cxx \
   cfdPresetVector.cxx \
   cfdReadParam.cxx \
   cfdScalarBarActor.cxx \
   cfdStreamers.cxx \
   cfdPolyData.cxx \
   cfdTeacher.cxx \
   cfdTransientInfo.cxx \
   cfdTransientSet.cxx \
   cfdTransientFlowManager.cxx \
   cfdTransientActor.cxx \
   cfdTransientParticleActor.cxx \
   cfdTransientScalarActor.cxx \
   cfdTransientVectorActor.cxx \
   cfdVectorBase.cxx \
   cfdVector.cxx \
   cfdVectors.cxx \
   cfdWriteTraverser.cxx \
   fileIO.cxx \
   readWriteVtkThings.cxx \
   cfdAccessoryFunctions.cxx \
   textPrompt.cxx \
   vtkActorToPF.cxx \
   cfdSound.cxx \
   cfdTransientParticleActor_fluent.cxx \
   VjObs_i.cxx \
   cfdDCS.cxx \
   cfdQuatCamHandler.cxx \
   cfdQuatCam.cxx \
   cfdSequence.cxx \
   cfdIHCCModel.cxx \
   cfdIHCCGauge.cxx \
   cfdIHCCContour.cxx \
   cfd1DTextInput.cxx

ifdef _CFDCOMMANDARRAY
SRCS+= \
   cfdGlobalBase.cxx \
   cfdCommandObjects.cxx \
   cfdCommandArray.cxx
endif

#   cfdDrawManager.cxx

ifeq ($(TAO_BUILD), TRUE)
SRCS+= \
   VjObsS.cpp \
   VjObsC.cpp \
   cfdExecutive.cxx \
   cfdExecutiveConfiguration.cxx \
   cfdGauges.cxx \
   cfdDigitalAnalogGauge.cxx \
   cfdDashboard.cxx \
   cfdSwappableGeometry.cxx \
   cfdInteractiveGeometry.cxx \
   cfdModuleGeometry.cxx \
   VE_i.cpp \
   moduleC.cpp \
   moduleS.cpp \
   interface.cpp \
   string_ops.cpp
else
SRCS+= \
   VjObs.cpp 
endif

#   Observer.cpp \
#   CorbaManager.cxx \
#   Conductor.cpp \
#   veConductorImpl.cpp

include $(VE_SUITE_HOME)/VE_Installer/dzr.vtk.mk

#LIBS=   $(LIBS_BEGIN) $(SYS_LIBS) $(LIBS_END)
#include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
#include $(DZR_BASE_DIR)/mk/dzr.java-app.mk

#include $(DZR_BASE_DIR)/ext/tweek/dzr.tweek.mk
#include $(TWEEK_BASE_DIR)/share/tweek/tweek.appdefs.mk

#__________________________________________________________
# Where is Paul Rajlich's vtkActorToPF directory?

#VTK_TO_PF    = ${VE_SUITE_HOME}/VE_XPlorer/vtkActorToPF

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

ifeq ($(CLUSTER_APP), TRUE)
   APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/project_cluster$(EXEC_TAG)
   EXTRA_CXXFLAGS+= -D_CLUSTER
   # object files are being stored in a system-dependent directory so that
   # builds for multiple platforms may be maintained simultaneously.

   #OBJDIR is the place that the object files are stored.
   OBJDIR=	$(CFDHOSTTYPE)_cluster$(EXEC_TAG)

   #DEPDIR is the place that the dependency files are stored.
   DEPDIR=	$(CFDHOSTTYPE)_cluster$(EXEC_TAG)
else
   APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/project$(EXEC_TAG)
   # object files are being stored in a system-dependent directory so that builds
   # for multiple platforms may be maintained simultaneously.

   #OBJDIR is the place that the object files are stored.
   OBJDIR=	$(CFDHOSTTYPE)$(EXEC_TAG)

   #DEPDIR is the place that the dependency files are stored.
   DEPDIR=	$(CFDHOSTTYPE)$(EXEC_TAG)
endif

default: all

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
all:
	mkdir -p $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)
#	$(MAKE) cxx_idl
#	$(MAKE) java_idl
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)
#	$(MAKE) java
#	$(MAKE) NetworkTestBean.jar
#	-$(MAKE) install

srcdir=		.

# turn off dependencies for idl stuff, make clean
NO_DEPEND=	YES

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk
include $(DZR_BASE_DIR)/ext/vrjuggler/dzr.vrjuggler.pfapp.mk

# list the cpp and h files here
#IDL_CXX_FILES= ${CONDUCTOR_BASE_DIR}/Observer.cpp \
               ${CONDUCTOR_BASE_DIR}/VjObs.cpp \
               ${CONDUCTOR_BASE_DIR}/Observer.h \
               ${CONDUCTOR_BASE_DIR}/VjObs.h 

# list the cpp and h files here
#IDL_CXX_FILES=	SliderSubject.cpp SliderSubject.h		\
		WhiteboardSubject.cpp WhiteboardSubject.h

# list the corresponding java files that are in directory "networktest"
#IDL_JAVA_FILES=	SliderSubject.java	\
		WhiteboardSubject.java

#JAVA_SRCS=  ObserverHolder.java \
            VjObsHolder.java \
            ObserverServant.java \
            send_param.java \
            ack_win.java \
            attrib.java \
            draw_baf.java \
            id_mapper.java \
            WholeNumberField.java \
            client.java \
            my_orb.java 
#            FileInterfaceHolder.java \
#            HelloHolder.java \
# list the corresponding java files that are in directory "networktest"
# These files correspond to the IDL_CXX_FILES listed above
#IDL_JAVA_FILES= Observer.java \
               VjObs.java 

# all class files are used in creating the jar files
#ALL_CLASSES= *.class
#EXTRA_SOURCEPATH_DIRS= ${CONDUCTOR_BASE_DIR}/java_stuff
#CLASSPATH= ${CONDUCTOR_BASE_DIR}/java_stuff
#CLASSPATH_ARG=	-classpath "${CONDUCTOR_BASE_DIR}/java_stuff"

# One of these must be uncommented.
DEBUG_APP?=		TRUE
#OPTIM_APP?=		TRUE

# vtkActorToPF needs VTK4
EXTRA_CXXFLAGS+=	-DVTK4 -DTABLET
#$(EXTRA_FLAGS) 

#EXTRA_DEBUG_FLAGS+=	-g -D_DEBUG -gslim 
EXTRA_OPTIM_FLAGS+=	-O2 -OPT:Olimit=0 -D_OPT

ifeq ($(TAO_BUILD), TRUE)
   EXTRA_CXXFLAGS+=	-D_TAO ${CPPFLAGS} -I$(BOOST_INCLUDES)
   EXTRA_LIBS+= ${ACE_LIBS} 
   #$(VLDLIBS)
else
   EXTRA_CXXFLAGS+=	$(TWEEK_CXX_FLAGS) $(CORBA_DEFINES) -I$(BOOST_INCLUDES)
   EXTRA_LIBS+= $(TWEEK_LIBS) $(TWEEK_EXTRA_LIBS) $(OMNILIB) 
endif

LINKALL_ON=		-all
LINKALL_OFF=		

STATIC_ON=		@APP_LINK_STATIC_ON@
STATIC_OFF=		@APP_LINK_STATIC_OFF@

#CLASSPATH=		$(TWEEK_JARS)

CXX_GEN_DIR=		.
#CXX_IDL=		$(TWEEK_IDL_CXX)
#CXX_IDL_OPTS=		$(TWEEK_IDLFLAGS_CXX)			\
			$(TWEEK_IDL_GENDIR_CXX)$(CXX_GEN_DIR)
#CXX_IDL_INCLUDES=	

#JAVA_GEN_DIR=		$(CONDUCTOR_BASE_DIR)/java_stuff
#JAVA_IDL=		$(TWEEK_IDL_JAVA)
#JAVA_IDL_OPTS=		$(TWEEK_IDLFLAGS_JAVA)			\
			$(TWEEK_IDL_GENDIR_JAVA)$(JAVA_GEN_DIR)
#JAVA_IDL_INCLUDES=	

#WARN_MSG= I_AM_HERE
#EXTRA_PATH_FOR_SOURCES= ${CONDUCTOR_BASE_DIR} \
                        ${CONDUCTOR_BASE_DIR}/java_stuff \
                        ${CONDUCTOR_BASE_DIR}/java_stuff/ObserverPackage \
                        ${CONDUCTOR_BASE_DIR}/java_stuff/VjObsPackage 

vpath %.cpp $(CXX_GEN_DIR) ${CONDUCTOR_BASE_DIR}/skel
#vpath %.cc ./UMI/skel
#vpath %.h $(CXX_GEN_DIR)  ${CONDUCTOR_BASE_DIR}/skel

#vpath %.idl $(CONDUCTOR_BASE_DIR)
#vpath %.java $(JAVA_GEN_DIR)

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
#cxx_idl: $(IDL_CXX_FILES)

#%.cpp %.h: %.idl
#	$(CXX_IDL) $(CXX_IDL_OPTS) $(CXX_IDL_INCLUDES) $<

#java_idl: $(IDL_JAVA_FILES)

#%.java: %.idl
#	$(JAVA_IDL) $(JAVA_IDL_OPTS) $(JAVA_IDL_INCLUDES) $<

cxx: $(OBJS)

#java: $(CLASSES)

${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)
#$(TWEEK_LIBS) 
#NetworkTestBean.jar: $(JAVA_CLASSES)
#	$(JAR) cvf $@ $(ALL_CLASSES)

install:
	cp NetworkTestBean.jar $(TWEEK_BASE_DIR)/bin/beans
	cp $(srcdir)/NetworkTestBean.xml $(TWEEK_BASE_DIR)/bin/beans

# in addition to the created *.o files and any core files, the following files will be removed during "gmake clean"
CLEAN_FILES+= $(APP_NAME)
#CLEAN_FILES+=	CorbaManager.o \
      $(addprefix $(srcdir)/, $(IDL_CXX_FILES))		\
		$(addprefix $(srcdir)/networktest/, $(IDL_JAVA_FILES))	\
      $(CONDUCTOR_BASE_DIR)/*.d \
		$(CLASSES) tweek/*.class

#  in addition to the "ii_files" directory, the following directories will be removed during "gmake clean"
#CLEAN_DIRS+=	$(srcdir)/tweek

#CLOBBER_FILES+=	NetworkTestBean.jar

# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.



# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
