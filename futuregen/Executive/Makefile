#----------------------------------------------------------------------------
#
#       Makefile,v 1.96 2002/09/09 00:27:13 crodrigu Exp
#
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
#       Local macros
#----------------------------------------------------------------------------

ifndef TAO_ROOT
  TAO_ROOT = $(ACE_ROOT)/TAO
endif # ! TAO_ROOT

#XERCESCROOT=/usr/local/xerces-c-src_2_5_0

EXECUTIVE_HOME = ../Executive
PLUGIN_HOME = ../Plugin

LDLIBS = -lTAO_CosNaming -lTAO_Svc_Utils -lTAO_IORTable -lTAO_Messaging -lTAO_PortableServer -lTAO -lxerces-c

include $(ACE_ROOT)/include/makeinclude/wrapper_macros.GNU
include $(TAO_ROOT)/rules.tao.GNU

include $(XERCESCROOT)/version.incl
include ../Plugin/Makefile.incl

IDL_FILES = ../IDL/module
IDL_SRC = ../IDL/moduleC.cpp ../IDL/moduleS.cpp

PROG_SRCS = $(IDL_SRC) \
            interface.cpp \
            package.cpp \
            Network_Exec.cpp \
            Scheduler.cpp \
            Exe_server.cpp \
            Executive_i.cpp \
            string_ops.cpp

LSRC = $(PROG_SRCS)

FILE_Exe_SVR_OBJS = moduleC.o moduleS.o Exe_server.o Executive_i.o interface.o package.o Network_Exec.o Scheduler.o string_ops.o

BIN_UNCHECKED = Exe_server

include $(ACE_ROOT)/include/makeinclude/macros.GNU
include $(ACE_ROOT)/include/makeinclude/rules.common.GNU
include $(ACE_ROOT)/include/makeinclude/rules.nonested.GNU
include $(ACE_ROOT)/include/makeinclude/rules.local.GNU
include $(TAO_ROOT)/taoconfig.mk

vpath %.cpp ${EXECUTIVE_HOME} ${PLUGIN_HOME}


TAO_IDLFLAGS += -Ge 1

LDFLAGS += -L$(TAO_ROOT)/tao -L$(TAO_ROOT)/orbsvcs/orbsvcs -L$(XERCESCROOT)/lib
CPPFLAGS += -I$(TAO_ROOT) -I$(TAO_ROOT)/orbsvcs \
            -I${EXECUTIVE_HOME} -I./. -I${PLUGIN_HOME} -LANG:std \
            -I$(XERCESCROOT)/include

#----------------------------------------------------------------------------
#       Include macros and targets
#----------------------------------------------------------------------------



#ifeq (cxvx,$(findstring cxvx,$(CXX)))

  # Green Hills for VxWorks can't build this test.  It runs into
  # trouble with template instantiations.
#  all clean realclean:
#	@echo POA/TIE is not built with Green Hills $(CXX).

#else # ! ghs/VxWorks
#ifeq (gcx,$(findstring gcx,$(CXX)))

  # Green Hills for Solaris can't build this test.  It runs into
  # trouble with template instantiations.
#  all clean realclean:
#	@echo POA/TIE is not built with Green Hills $(CXX).

#else # ! ghs/VxWorks && ! ghs/Solaris

#include $(ACE_ROOT)/include/makeinclude/macros.GNU
#include $(TAO_ROOT)/rules.tao.GNU

#ifeq ($(minimum_corba),0)
#  ifneq (Compilers 4.2,$(findstring Compilers 4.2,$(CC_VERSION)))
#    BIN = $(BIN_UNCHECKED)
#  endif # findstring Compilers 4.2
#endif # minimum_corba

#include $(ACE_ROOT)/include/makeinclude/rules.common.GNU
#include $(ACE_ROOT)/include/makeinclude/rules.nonested.GNU
#include $(ACE_ROOT)/include/makeinclude/rules.bin.GNU
#include $(ACE_ROOT)/include/makeinclude/rules.local.GNU
#include $(TAO_ROOT)/taoconfig.mk

.PRECIOUS: moduleC.h moduleC.i moduleC.cpp moduleS.h moduleS.i moduleS.cpp

all: Exe_server

Exe_server: $(addprefix $(VDIR),$(FILE_Exe_SVR_OBJS))
	$(LINK.cc) $(LDFLAGS) -o $@ $^ $(TAO_SRVR_LIBS) $(VLDLIBS) $(POSTLINK)

realclean: clean
	-$(RM) -r moduleC.* moduleS.* mdule_T.* ./.obj Exe_server

#endif # ! ghs/Solaris
#endif # ! ghs/VxWorks

#----------------------------------------------------------------------------
#       Dependencies
#----------------------------------------------------------------------------

# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.


# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
