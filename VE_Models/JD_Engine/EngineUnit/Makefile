IDL_HOME = ../../IDL
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

include ${VE_SUITE_HOME}/futuregen/Unit/Makefile.incl

ifndef TAO_ROOT
  TAO_ROOT = $(ACE_ROOT)/TAO
endif # ! TAO_ROOT

APP = CoolingUnitApp

LDLIBS = -lTAO_CosNaming -lTAO_Svc_Utils -lTAO_IORTable -lTAO_Messaging -lTAO_PortableServer -lTAO

include $(ACE_ROOT)/include/makeinclude/wrapper_macros.GNU
include $(TAO_ROOT)/rules.tao.GNU

FILE_UNIT_CLT_OBJS = moduleC.o moduleS.o EngineUnit_i.o \
                     EngineUnit_client.o

include $(ACE_ROOT)/include/makeinclude/macros.GNU
include $(ACE_ROOT)/include/makeinclude/rules.common.GNU
include $(ACE_ROOT)/include/makeinclude/rules.nonested.GNU
include $(ACE_ROOT)/include/makeinclude/rules.local.GNU
include $(TAO_ROOT)/taoconfig.mk

vpath %.cpp ${VE_SUITE_HOME}/futuregen/IDL ${VE_SUITE_HOME}/futuregen/Plugin


LDFLAGS += -L$(TAO_ROOT)/tao -L$(TAO_ROOT)/orbsvcs/orbsvcs

CPPFLAGS += -g  ${INCLUDES} ${CMP} -I$(TAO_ROOT) \
		-I$(TAO_ROOT)/orbsvcs -I${VE_SUITE_HOME}/futuregen/IDL \
		-I./. -I${VE_SUITE_HOME}/futuregen/Plugin

.PRECIOUS: moduleC.i moduleC.cpp moduleS.i moduleS.cpp

OBJECTS = string_ops.o \
	      interface.o \
	      package.o 

.SUFFIXES : .o .cpp .cc	

.cpp.o :
	$(CXX) -g -c $(CPPFLAGS) -w -o $@ $<

.cc.o :
	$(CXX) -g -c $(CPPFLAGS) -w -o $@ $<

all: $(APP)

$(APP): $(OBJECTS) $(addprefix $(VDIR),$(FILE_UNIT_CLT_OBJS))
	g++ ${PLATFORM_LIB_LINK_OPTIONS} $(LDFLAGS) -o $@ $^ $(TAO_SRVR_LIBS) $(VLDLIBS) ${LIBRARY_SEARCH_PATHS} ${LIBRARY_NAMES} ${EXTRA_LINK_OPTIONS} $(POSTLINK)

clean:
	rm -rf *.o $(APP) $(FILE_UNIT_CLT_OBJS) $(OBJECTS)
