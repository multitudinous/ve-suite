# Create the OS-dependent directory name
CFDHOSTTYPE?=	$(DZR_HOSTTYPE)
VE_SUITE_HOME?= ../

APP_NAME= WebServices

IDL_HOME = $(VE_SUITE_HOME)/VEOpen

srcdir= ./src/

all:
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

SRCS=\
   cfdWebServicesMain.cxx \
   cfdWebServices.cxx \
   moduleC.cpp \
   moduleS.cpp \
   veWebService_i.cpp \
	query.cpp	\
	vallist.cpp	\
	field_names.cpp	\
	field_types.cpp	\
	row.cpp			\
	manip.cpp		\
	sql_query.cpp	\
	string_util.cpp	\
	fields.cpp		\
	type_info.cpp	\
	myset.cpp		\
	result.cpp		\
	sql_string.cpp	\
	coldata.cpp		\
	datetime.cpp	\
	connection.cpp	\
   
#the SQL cpp's really shouldn't be here...having trouble figuring out what library it's built into though.
   

# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
#DEBUG_APP?= TRUE
OPTIM_APP?= TRUE

EXTRA_CFLAGS+=

EXTRA_CXXFLAGS+=

EXTRA_DEFS+=

EXTRA_LIBS+=

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.wx.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ace_tao.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.xerces.mk

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk

EXEC_TAG = _tao
vpath %.cpp $(VE_SUITE_HOME)	\
         $(VE_SUITE_HOME)/VE_Conductor/Framework \
			$(VE_SUITE_HOME)/VE_Xplorer \
			$(VE_SUITE_HOME)/VE_Conductor/VE_UI $(IDL_HOME)/skel \
			$(VE_SUITE_HOME)/VE_SceneGraph \
			$(VE_SUITE_HOME)/VE_CE			\
			$(VE_SUITE_HOME)/../msqlpp/lib			\
			
#again, that last one shouldn't be there.  hopefully I can find the library for mysqlpp::connection and get rid of this
EXTRA_CXXFLAGS += -I$(VE_SUITE_HOME)/VE_Conductor/Framework -I$(IDL_HOME)/skel 
EXTRA_CXXFLAGS += -I$(VE_SUITE_HOME)/VE_Conductor/VE_UI 
EXTRA_CXXFLAGS += -I/usr/include/mysql
EXTRA_CXXFLAGS += -I/home/users/kennyk/Graphics_Programming/mysql++
EXTRA_LIBS +=	-B static /usr/lib/mysql/libmysqlclient.a
EXTRA_LIBS +=	-B static /usr/lib/mysql/libmysys.a
EXTRA_LIBS +=	-B static /usr/lib/mysql/libmystrings.a
EXTRA_LIBS +=	-B /home/users/kennyk/Graphics_Programming/msqlpp/lib/libmysqlpp.la
EXTRA_LIBS += -lVE_UtilLib.so -lGUIPluginLib.so -lVE_ConductorLib.so

EXTRA_CXXFLAGS += -I$(VJ_BASE_DIR)/include -I$(VE_SUITE_HOME)/VE_CE -I$(VE_SUITE_HOME)/VE_Xplorer -I$(VE_SUITE_HOME)/VE_SceneGraph 


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
#	cp NetworkTestBean.jar $(TWEEK_BASE_DIR)/bin/beans
#	cp $(srcdir)/NetworkTestBean.xml $(TWEEK_BASE_DIR)/bin/beans

CLEAN_FILES+= $(APP_NAME)
