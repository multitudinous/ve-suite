#__________________________________________________________
# MAKEFILE
#__________________________________________________________

LIBS=   $(LIBS_BEGIN) $(SYS_LIBS) $(LIBS_END)

# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

default: all

all:
	$(MAKE) cxx_idl
	$(MAKE) java_idl
	$(MAKE) java

srcdir=		.

# turn off dependencies for idl stuff, make clean
NO_DEPEND=	YES

CONDUCTOR_BASE_DIR= ${VE_SUITE_HOME}/VE_Conductor
#JDK_HOME = /usr/java2
# list the cpp and h files here
IDL_CXX_FILES= Observer.h \
               Observer.cpp \
               VjObs.h \
               VjObs.cpp 

JAVA_SRCS=  ObserverHolder.java \
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

# list the corresponding java files that are in directory "networktest"
# These files correspond to the IDL_CXX_FILES listed above
IDL_JAVA_FILES= VjObs.java \
                Observer.java 

# all class files are used in creating the jar files
#ALL_CLASSES= class_files/*.class
JAVA_CLASSDIR= $(srcdir)/class_files
EXTRA_SOURCEPATH_DIRS= $(srcdir)/java_stuff
#CLASSPATH= ${CONDUCTOR_BASE_DIR}/java_stuff
#CLASSPATH_ARG=	-classpath "${CONDUCTOR_BASE_DIR}/java_stuff"
include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
include $(DZR_BASE_DIR)/mk/dzr.java-app.mk
include $(DZR_BASE_DIR)/ext/tweek/dzr.tweek.mk
#include $(TWEEK_BASE_DIR)/share/tweek/tweek.appdefs.mk

# One of these must be uncommented.
DEBUG_APP?=		TRUE
#OPTIM_APP?=		TRUE

LINKALL_ON=		-all
LINKALL_OFF=		

STATIC_ON=		@APP_LINK_STATIC_ON@
STATIC_OFF=		@APP_LINK_STATIC_OFF@

CLASSPATH=		$(TWEEK_JARS)

CXX_GEN_DIR=	$(srcdir)/skel
ifeq (${TAO_BUILD}, TRUE)
   CXX_IDL=		${TAO_ROOT}/TAO_IDL/tao_idl
   CXX_IDL_OPTS=  -o $(CXX_GEN_DIR) -GI -I./idl
else
   CXX_IDL=		omniidl
   #CXX_IDL=		$(TWEEK_IDL_CXX)
   CXX_IDL_OPTS=		$(TWEEK_IDLFLAGS_CXX)			\
			$(TWEEK_IDL_GENDIR_CXX)$(CXX_GEN_DIR)
endif
CXX_IDL_INCLUDES=	

JAVA_GEN_DIR=		$(srcdir)/java_stuff
JAVA_IDL=		idlj
#JAVA_IDL=		$(TWEEK_IDL_JAVA)
JAVA_IDL_OPTS=		-fall $(TWEEK_IDLFLAGS_JAVA)			\
			-td $(JAVA_GEN_DIR)
#			$(TWEEK_IDL_GENDIR_JAVA)$(JAVA_GEN_DIR)
JAVA_IDL_INCLUDES=	-I$(CONDUCTOR_BASE_DIR)/idl

vpath %.idl $(srcdir)/idl
vpath %.java $(JAVA_GEN_DIR) $(JAVA_GEN_DIR)/VjObsPackage \
               $(JAVA_GEN_DIR)/ObserverPackage


# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx_idl: $(IDL_CXX_FILES)

%.cpp %.h: %.idl
	$(CXX_IDL) $(CXX_IDL_OPTS) $(CXX_IDL_INCLUDES) $<

java_idl: $(IDL_JAVA_FILES)

%.java: %.idl
	$(JAVA_IDL) $(JAVA_IDL_OPTS) $(JAVA_IDL_INCLUDES) $<

java: $(JAVA_CLASSES)

# in addition to the created *.o files and any core files, the following files will be removed during "gmake clean"
CLEAN_FILES+=	\
      $(addprefix $(CXX_GEN_DIR)/, $(IDL_CXX_FILES)) \
		$(addprefix $(JAVA_GEN_DIR)/, $(IDL_JAVA_FILES))	\
		${JAVA_CLASSDIR}/*.class ${CXX_GEN_DIR}/*.cpp ${CXX_GEN_DIR}/*.h

#  in addition to the "ii_files" directory, the following directories will be removed during "gmake clean"
CLEAN_DIRS+=	$(JAVA_GEN_DIR)/VjObsPackage $(JAVA_GEN_DIR)/ObserverPackage \
               $(JAVA_CLASSDIR)/VjObsPackage $(JAVA_CLASSDIR)/ObserverPackage

