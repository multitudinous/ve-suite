EXTRA_CXXFLAGS+= -D_OSG 
include $(DZR_BASE_DIR)/ext/vrjuggler/dzr.vrjuggler.glapp.mk
EXTRA_INCLUDES+= -I$(OSG_HOME)/include
CFDUNAME = $(shell uname)
ifeq (${CFDUNAME},Linux)
   CFDPLATFORM = $(shell uname -m)
else
   CFDPLATFORM = $(shell uname)
endif

ifeq (${CFDPLATFORM},x86_64)
   EXTRA_LIBS+= -L$(OSG_HOME)/lib -L$(OSG_HOME)/lib64 -losg -losgDB -losgGA -losgUtil \
            -lOpenThreads -losgFX -losgText -losgSim -losgProducer -lProducer
   DSO_PLUGIN_DEPS+= -L$(OSG_HOME)/lib -L$(OSG_HOME)/lib64 -losg -losgDB -losgGA -losgUtil \
            -lOpenThreads -losgFX -losgText -losgSim -losgProducer -lProducer
else
   ifeq (${CFDUNAME},IRIX64)
      EXTRA_LIBS+= -L$(OSG_HOME)/lib32 -losg -losgDB -losgGA -losgUtil \
            -lOpenThreads -losgFX -losgText -losgSim -losgProducer -lProducer
   else
      EXTRA_LIBS+= -L$(OSG_HOME)/lib -losg -losgDB -losgGA -losgUtil \
            -lOpenThreads -losgText -losgSim -losgProducer -lProducer
      DSO_PLUGIN_DEPS+= -L$(OSG_HOME)/lib -losg -losgDB -losgGA -losgUtil \
            -lOpenThreads -losgText -losgSim -losgProducer -lProducer
   endif
endif
