CXX = $(shell wx-config --cxx)
WX_CXX = $(shell wx-config --cxxflags)
WX_LIBS = $(shell wx-config --libs)
VE_UI_HOME = ./../VE_UI
IDL_HOME = ./../IDL

PLATFORM = LINUX
COMPILER = g++
GCC = yes
GXX = yes
CXXFLAGS =   -g -w -O  -DAPP_NO_THREADS -DXML_USE_NO_THREADS  
CFLAGS =   -g -w -O  -DAPP_NO_THREADS -DXML_USE_NO_THREADS  
LDFLAGS =    
THREADS = none
EXTRA_LIBS =  
PREFIX = /usr/local

#include ${XERCESCROOT}/version.incl
include ../Plugin/Makefile.incl

ifndef TAO_ROOT
  TAO_ROOT = $(ACE_ROOT)/TAO
endif # ! TAO_ROOT

APP = app

LDLIBS = -lTAO_CosNaming -lTAO_Svc_Utils -lTAO_IORTable -lTAO_Messaging -lTAO_PortableServer -lTAO

include $(ACE_ROOT)/include/makeinclude/wrapper_macros.GNU
include $(TAO_ROOT)/rules.tao.GNU

FILE_UI_CLT_OBJS = moduleC.o moduleS.o UI_i.o 

include $(ACE_ROOT)/include/makeinclude/macros.GNU
include $(ACE_ROOT)/include/makeinclude/rules.common.GNU
include $(ACE_ROOT)/include/makeinclude/rules.nonested.GNU
include $(ACE_ROOT)/include/makeinclude/rules.local.GNU
include $(TAO_ROOT)/taoconfig.mk

vpath %.cpp ${VE_UI_HOME} ${IDL_HOME}

LDFLAGS += -L$(TAO_ROOT)/tao -L$(TAO_ROOT)/orbsvcs/orbsvcs

CPPFLAGS += -g  ${INCLUDES} ${CMP} -I../Plugin ${WX_CXX} -I$(TAO_ROOT) \
		-I$(TAO_ROOT)/orbsvcs -I${VE_UI_HOME} -I${IDL_HOME} \
		-I./. #-LANG:std 

.PRECIOUS: moduleC.i moduleC.cpp moduleS.i moduleS.cpp


OBJECTS = StringParse.o \
	OrbThread.o \
	VjObsC.o \
	Frame.o \
	App.o \
	PluginLoader.o \
	Network.o \
	Avail_Modules.o \
	ListTable.o \
	PortDialog.o \
	ResultPanel.o \
	GlobalParamDialog.o \
	paraThread.o \
	../Plugin/string_ops.o \
	../Plugin/interface.o \
	../Plugin/package.o \
	../Plugin/Plugin_base.o \
	../Plugin/UIDialog.o \
	../Plugin/TexTable.o \
	../Plugin/TextResultDialog.o \
	UI_Tabs.o \
	UI_VisTab.o \
	UI_ScalarTab.o \
	UI_VecTab.o \
	UI_StreamTab.o \
	UI_DataSetTab.o \
	UI_SoundsTab.o \
	UI_GeometryTab.o \
	UI_TeacherTab.o \
	UI_NavTab.o  \
	UI_TransTab.o \
	UI_VertTab.o \
	UI_DesignParTab.o \
	UI_ViewLocTab.o

.SUFFIXES : .o .cc .cpp

.cc.o :
	$(CXX) -g -c $(CPPFLAGS) -w -D WXUSING_PLUGIN_DLL -o $@ $<

.cpp.o :
	$(CXX) -g -c $(CPPFLAGS) -w -D WXUSING_PLUGIN_DLL -o $@ $<

all: $(APP)

$(APP): $(OBJECTS) $(addprefix $(VDIR),$(FILE_UI_CLT_OBJS))
	g++ ${PLATFORM_LIB_LINK_OPTIONS} $(LDFLAGS) -o $@ $^ ${WX_LIBS} $(TAO_SRVR_LIBS) $(VLDLIBS) ${LIBRARY_SEARCH_PATHS} ${LIBRARY_NAMES} ${EXTRA_LINK_OPTIONS} $(POSTLINK)

clean:
	rm -rf *.o $(APP) ./.obj ./ii_files ./Debug
