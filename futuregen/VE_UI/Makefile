CXX = $(shell wx-config --cxx)
WX_CXX = $(shell wx-config --cxxflags)
WX_LIB = $(shell wx-config --libs)

APP = app

OBJECTS = UI_App.o \
            UI_Frame.o \
            UI_Tabs.o \
            UI_NavTab.o\
            UI_VisTab.o \
            UI_VecTab.o \
            UI_StreamTab.o \
            UI_SoundsTab.o \
            UI_GeometryTab.o \
            UI_TeacherTab.o \
            UI_TransTab.o \
            UI_VertTab.o \
            UI_ViewLocTab.o \
            UI_DesignParTab.o \
            UI_DataSetPanel.o

ifeq (${TAO_BUILD}, TRUE)            
   OBJECTS += VjObsC.o
   include ${ACE_ROOT}/include/makeinclude/wrapper_macros.GNU
   include ${ACE_ROOT}/include/makeinclude/macros.GNU
#   include ${TAO_ROOT}/rules.tao.GNU
   include ${ACE_ROOT}/include/makeinclude/rules.common.GNU
   include ${ACE_ROOT}/include/makeinclude/rules.nonested.GNU
   include ${ACE_ROOT}/include/makeinclude/rules.local.GNU
   include ${TAO_ROOT}/taoconfig.mk

   ACE_LIBS = -LANG:std -L${ACE_ROOT}/ace -lACE \
               -lTAO_CosNaming \
               -lTAO_Svc_Utils \
               -lTAO_IORTable \
               -lTAO_PortableServer \
               -lTAO_Messaging \
               -lTAO_ObjRefTemplate \
               -lTAO_IORInterceptor \
               -lTAO_Valuetype \
               -L${TAO_ROOT}/tao -lTAO -lpthread
#${LDFLAGS}
   ACE_INCLUDES = -I${TAO_ROOT}/orbsvcs
   ENUM_INCLUDES = -I${VE_SUITE_HOME}/VE_Xplorer
   CPP_FLAGS = -LANG:std -D_TAO -I${VE_SUITE_HOME}/VE_Conductor/skel ${ACE_INCLUDES} ${ENUM_INCLUDES} ${CPPFLAGS}
   vpath %.cpp    ${VE_SUITE_HOME}/VE_Conductor/skel
endif

.SUFFIXES : .o .cpp

.cpp.o :
	$(CXX) -g -c  $(WX_CXX) $(CPP_FLAGS) -o $@ $<

all: $(APP)

$(APP): $(OBJECTS)
	$(CXX) -g -o $(APP) $(OBJECTS) $(WX_LIB) $(ACE_LIBS)

clean:
	rm -rf *.o $(APP) ii_files

# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.



# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
