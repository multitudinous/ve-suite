################### ace_tao-specific Makefile ###################
CFDUNAME = $(shell uname)

ifeq (${CFDUNAME},Linux)
EXTRA_CXXFLAGS+= -W -Wall -Wpointer-arith -pipe -O3 -g -D_TAO -D_REENTRANT -DACE_HAS_AIO_CALLS -D_GNU_SOURCE -DACE_HAS_EXCEPTIONS -D__ACE_INLINE__
EXTRA_LIBS+=  -L${TAO_ROOT}/Linux/lib -L${ACE_ROOT}/Linux/lib 
EXTRA_INCLUDES+= -I${TAO_ROOT}/Linux/include -I${ACE_ROOT}/Linux/include 
endif

ifeq (${CFDUNAME},IRIX64)
EXTRA_CXXFLAGS+=  -DACE_HAS_EXCEPTIONS -DACE_IRIX_VERS=65 -D_SGI_MP_SOURCE -DACE_HAS_XT  -D__ACE_INLINE__
EXTRA_LIBS+=  -L${TAO_ROOT}/IRIX32/lib -L${ACE_ROOT}/IRIX32/lib 
EXTRA_INCLUDES+= -I${TAO_ROOT}/IRIX32/include -I${ACE_ROOT}/IRIX32/include 
endif

EXTRA_LIBS+= -lTAO_PortableServer -lTAO_IORInterceptor -lTAO_ObjRefTemplate -lTAO_Valuetype \
               -lTAO -lACE -lpthread -lTAO_CosNaming -lTAO_Svc_Utils -lTAO_IORTable \
               -lTAO_Messaging -lTAO_PortableServer -lpthread -lTAO_BiDirGIOP

CPPFLAGS+= ${EXTRA_CXXFLAGS} ${EXTRA_INCLUDES}
EXTRA_INCLUDES+= -I${TAO_ROOT}/include -I${ACE_ROOT}/include 


LDFLAGS+= ${EXTRA_LIBS}
