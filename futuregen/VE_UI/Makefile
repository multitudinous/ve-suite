CXX = $(shell wx-config --cxx)
WX_CXX = $(shell wx-config --cxxflags)
WX_LIB = $(shell wx-config --libs)

APP = app

#OBJDIR is the place where the object files are stored.
OBJDIR = $(CFDHOSTTYPE)

OBJECT_LIST = UI_App.o \
              UI_TopFrame.o \
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
              UI_DataSetPanel.o \
              UI_ModSelPanel.o \
              UI_ModelData.o

OBJECTS = $(addprefix $(OBJDIR)/, $(OBJECT_LIST))

ifeq (${TAO_BUILD}, TRUE)
   OBJECT_LIST += VjObsC.o
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

# implicit pattern rules
$(OBJDIR)/%.o : %.cxx ; $(CXX) -g -c $(WX_CXX) $(CPP_FLAGS) -o $@ $<
$(OBJDIR)/%.o : %.cpp ; $(CXX) -g -c $(WX_CXX) $(CPP_FLAGS) -o $@ $<

all:
	mkdir -p $(OBJDIR)
	$(MAKE) $(APP)

$(APP): $(OBJECTS)
	$(CXX) -g -o $(APP) $(OBJECTS) $(WX_LIB) $(ACE_LIBS)

clean:
	rm -rf $(OBJDIR) $(APP)

cleandepend:
#	@echo "    There are no depends to clean with this makefile"
#	@echo ""
# (defined for compatability with VE_Installer)

