#_______________________________________________________________________________
# MAKEFILE
#_______________________________________________________________________________
#

CONDUCTOR_BASE_DIR= ${VE_SUITE_HOME}/VE_Conductor
CFDUNAME = $(shell uname)

# If OMNI_HOME was defined in the script setup.tsh, 
# then define CORBA_DEFINES, and CORBA_INCLUDES.
# (To make a non-corba version, don't define OMNI_HOME.)
ifdef OMNI_HOME
   vpath %.cxx    ${CONDUCTOR_BASE_DIR}/impl

   CORBA_DEFINES = -D__OSVERSION__=6 

   ifeq ($(CFDUNAME),Linux)
      CORBA_DEFINES += -D__x86__ -D__linux__
      OMNI_LIB = ${OMNI_HOME}/lib
   else
      CORBA_DEFINES += -D__mips__ -D__irix__
      OMNI_LIB = ${OMNI_HOME}/lib
   endif

   OMNILIB = -L${OMNI_LIB} -lomniORB4 -lomnithread -lomniDynamic4
   
   CORBA_INCLUDES = -I${OMNI_HOME}/include 
   EXTRA_INCLUDES+= -I${CONDUCTOR_BASE_DIR}/impl \
                    -I${CONDUCTOR_BASE_DIR}/skel \
                     ${CORBA_INCLUDES}
endif

ifdef ACE_ROOT
   FUTUREGEN_HOME = /home/users/mccdo/REI/futuregen

   vpath %.cxx    ${CONDUCTOR_BASE_DIR}/tao_impl 
   vpath %.cpp    ${FUTUREGEN_HOME}/IDL.old ${FUTUREGEN_HOME}/Plugin
   vpath %.h      ${CONDUCTOR_BASE_DIR}/tao_impl ${FUTUREGEN_HOME}/IDL.old
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
   EXTRA_INCLUDES+= \
                  ${ACE_INCLUDES} \
                  -I${CONDUCTOR_BASE_DIR}/skel \
                  -I${CONDUCTOR_BASE_DIR}/tao_impl \
                  -I${FUTUREGEN_HOME}/IDL.old \
                  -I${FUTUREGEN_HOME}/Plugin
endif

#__________________________________________________________
# Operating System Specific stuff
ifeq ($(CFDUNAME),Linux)
 #  EXTRA_FLAGS = -O2 -DHAVE_CONFIG_H
endif

# list the source files that are in current directory
SRCS=\
   cfdVeModel.cpp \
   cfdVeView.cpp \
   cfdVeController.cpp \
   cfdSubjectBase.cpp \
   cfdObserverBase.cpp \
   VjDesignObs.cpp

#   cfdDrawManager.cxx

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

# object files are being stored in a system-dependent directory so that builds
# for multiple platforms may be maintained simultaneously.

#OBJDIR is the place that the object files are stored.
OBJDIR= $(CFDHOSTTYPE)

#DEPDIR is the place that the dependency files are stored.
DEPDIR= $(CFDHOSTTYPE)

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

APP_NAME=project

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

srcdir= .

# turn off dependencies for idl stuff, make clean
NO_DEPEND= YES

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk
include $(DZR_BASE_DIR)/ext/vrjuggler/dzr.vrjuggler.pfapp.mk

# list the cpp and h files here
#IDL_CXX_FILES= ${CONDUCTOR_BASE_DIR}/Observer.cpp \
               ${CONDUCTOR_BASE_DIR}/VjObs.cpp \
               ${CONDUCTOR_BASE_DIR}/Observer.h \
               ${CONDUCTOR_BASE_DIR}/VjObs.h 

# list the cpp and h files here
#IDL_CXX_FILES= SliderSubject.cpp SliderSubject.h \
WhiteboardSubject.cpp WhiteboardSubject.h

# list the corresponding java files that are in directory "networktest"
#IDL_JAVA_FILES= SliderSubject.java \
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
#CLASSPATH_ARG= -classpath "${CONDUCTOR_BASE_DIR}/java_stuff"

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

EXTRA_CXXFLAGS+= -DTABLET
EXTRA_LIBS+= -L${HOME}/software/IRIX/lib -lcppdom
EXTRA_INCLUDES+= -I${HOME}/software/IRIX/include
#$(EXTRA_FLAGS) 

#EXTRA_DEBUG_FLAGS+= -g -D_DEBUG -gslim 
EXTRA_OPTIM_FLAGS+= -O2 -OPT:Olimit=0 -D_OPT
ifeq ($(TAO_BUILD), TRUE)
   EXTRA_CXXFLAGS+= -D_TAO ${CPPFLAGS}
   EXTRA_LIBS+= ${ACE_LIBS} 
   #$(VLDLIBS)
else
   EXTRA_CXXFLAGS+= $(TWEEK_CXX_FLAGS) $(CORBA_DEFINES)
   EXTRA_LIBS+= $(TWEEK_LIBS) $(TWEEK_EXTRA_LIBS) $(OMNILIB) 
endif

LINKALL_ON= -all
LINKALL_OFF= 

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

#CLASSPATH= $(TWEEK_JARS)

CXX_GEN_DIR= .
#CXX_IDL= $(TWEEK_IDL_CXX)
#CXX_IDL_OPTS= $(TWEEK_IDLFLAGS_CXX) \
 $(TWEEK_IDL_GENDIR_CXX)$(CXX_GEN_DIR)
#CXX_IDL_INCLUDES= 

#JAVA_GEN_DIR= $(CONDUCTOR_BASE_DIR)/java_stuff
#JAVA_IDL= $(TWEEK_IDL_JAVA)
#JAVA_IDL_OPTS= $(TWEEK_IDLFLAGS_JAVA) \
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
#CLEAN_FILES+= $(APP_NAME) *.d
#CLEAN_FILES+= CorbaManager.o \
               $(addprefix $(srcdir)/, $(IDL_CXX_FILES)) \
               $(addprefix $(srcdir)/networktest/, $(IDL_JAVA_FILES)) \
               $(CONDUCTOR_BASE_DIR)/*.d \
               $(CLASSES) tweek/*.class

#  in addition to the "ii_files" directory, the following directories will be removed during "gmake clean"
#CLEAN_DIRS+= $(srcdir)/tweek

#CLOBBER_FILES+= NetworkTestBean.jar

# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.



# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
