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

LDLIBS = -lTAO_CosNaming -lTAO_Svc_Utils -lTAO_IORTable -lTAO_Messaging -lTAO_PortableServer -lTAO

include $(ACE_ROOT)/include/makeinclude/wrapper_macros.GNU
include $(TAO_ROOT)/rules.tao.GNU

IDL_FILES = module
IDL_SRC = moduleC.cpp moduleS.cpp

FILE_OBJS = moduleC.o moduleS.o 

include $(ACE_ROOT)/include/makeinclude/macros.GNU
include $(ACE_ROOT)/include/makeinclude/rules.common.GNU
include $(ACE_ROOT)/include/makeinclude/rules.nonested.GNU
include $(ACE_ROOT)/include/makeinclude/rules.local.GNU
include $(TAO_ROOT)/taoconfig.mk

TAO_IDLFLAGS += -Ge 1

LDFLAGS += -L$(TAO_ROOT)/tao -L$(TAO_ROOT)/orbsvcs/orbsvcs
CPPFLAGS += -I$(TAO_ROOT) -I$(TAO_ROOT)/orbsvcs

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

all : $(addprefix $(VDIR),$(FILE_OBJS))
	


realclean: clean
	-$(RM) moduleC.* moduleS.* mdule_T.*

#endif # ! ghs/Solaris
#endif # ! ghs/VxWorks

#----------------------------------------------------------------------------
#       Dependencies
#----------------------------------------------------------------------------

# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.


# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
