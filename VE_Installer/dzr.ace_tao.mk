################### ace_tao-specific Makefile ###################
CFDUNAME = $(shell uname)

ifeq (${CFDUNAME},Linux)
EXTRA_CXXFLAGS+= -W -Wall -Wpointer-arith -pipe -O3 -g -D_TAO -D_REENTRANT -DACE_HAS_AIO_CALLS -D_GNU_SOURCE -DACE_HAS_EXCEPTIONS -D__ACE_INLINE__
EXTRA_LIBS+=  -L${TAO_HOME}/Linux/lib -L${ACE_HOME}/Linux/lib 
EXTRA_INCLUDES+= -I${TAO_HOME}/Linux/include -I${ACE_HOME}/Linux/include 
endif

ifeq (${CFDUNAME},IRIX64)
EXTRA_CXXFLAGS+= -D_TAO -DACE_HAS_EXCEPTIONS -DACE_IRIX_VERS=65 -D_SGI_MP_SOURCE -DACE_HAS_XT  -D__ACE_INLINE__
EXTRA_LIBS+=  -L${TAO_HOME}/IRIX32/lib -L${ACE_HOME}/IRIX32/lib 
EXTRA_INCLUDES+= -I${TAO_HOME}/IRIX32/include -I${ACE_HOME}/IRIX32/include 
endif

EXTRA_LIBS+= -lTAO_IORInterceptor -lTAO_ObjRefTemplate -lTAO_Valuetype \
               -lTAO -lACE -lpthread -lTAO_CosNaming -lTAO_Svc_Utils -lTAO_IORTable \
               -lTAO_Messaging -lTAO_PortableServer -lTAO_BiDirGIOP

EXTRA_INCLUDES+= -I${TAO_HOME}/include -I${ACE_HOME}/include 


LDFLAGS+= ${EXTRA_LIBS}
