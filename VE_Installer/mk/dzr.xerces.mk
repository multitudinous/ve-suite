EXTRA_INCLUDES+= -I${XERCESCROOT}/include
EXTRA_LIBS+= -L${XERCESCROOT}/lib -lxerces-c

DSO_PLUGIN_DEPS+= -L${XERCESCROOT}/lib -lxerces-c -lxerces-depdom
