################### ace_tao-specific Makefile ###################
EXTRA_CXXFLAGS+= -D_TAO $(shell flagpoll ACE TAO --cflags)
EXTRA_LIBS+=  $(shell flagpoll ACE TAO TAO_IORInterceptor TAO_ObjRefTemplate TAO_Valuetype \
                              TAO_CosNaming TAO_Svc_Utils TAO_IORTable TAO_Messaging \
                              TAO_PortableServer TAO_BiDirGIOP TAO_AnyTypeCode --libs)

DSO_PLUGIN_DEPS+=  $(shell flagpoll ACE TAO TAO_IORInterceptor TAO_ObjRefTemplate TAO_Valuetype \
                              TAO_CosNaming TAO_Svc_Utils TAO_IORTable TAO_Messaging \
                              TAO_PortableServer TAO_BiDirGIOP TAO_AnyTypeCode --libs)

LDFLAGS+= ${EXTRA_LIBS}

