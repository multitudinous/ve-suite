EXTRA_CXXFLAGS+= -D_OSG -DUSE_DEPRECATED_API
include $(DZR_BASE_DIR)/ext/vrjuggler/dzr.vrjuggler.glapp.mk
EXTRA_INCLUDES+= -I$(OSG_HOME)/include

ifeq (${CFDUNAME},IRIX64)
   EXTRA_LIBS+= -L$(OSG_HOME)/lib32 -losg -losgDB -losgGA -losgUtil \
            -lOpenThreads -losgFX -losgGL2
else
   EXTRA_LIBS+= -L$(OSG_HOME)/lib -losg -losgDB -losgGA -losgUtil \
            -lOpenThreads -losgFX -losgGL2
endif
