EXTRA_CXXFLAGS+= -D_OSG 
include $(DZR_BASE_DIR)/ext/vrjuggler/dzr.vrjuggler.glapp.mk
EXTRA_INCLUDES+= -I$(OSG_HOME)/include
CFDPLATFORM = $(shell uname -i)

ifeq (${CFDPLATFORM},x86_64)
   EXTRA_LIBS+= -L$(OSG_HOME)/lib -L$(OSG_HOME)/lib64 -losg -losgDB -losgGA -losgUtil \
            -lOpenThreads -losgFX 

else
   ifeq (${CFDUNAME},IRIX64)
      EXTRA_LIBS+= -L$(OSG_HOME)/lib32 -losg -losgDB -losgGA -losgUtil \
            -lOpenThreads -losgFX 
   else
      EXTRA_LIBS+= -L$(OSG_HOME)/lib -losg -losgDB -losgGA -losgUtil \
            -lOpenThreads -losgFX 
   endif

endif
