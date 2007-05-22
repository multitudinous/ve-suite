TOPDIR = ../..
include $(TOPDIR)/Make/makedefs 

CXXFILES =\
	IO_oqNode.cpp\
	IO_oqRoot.cpp\

DEF     += -DOSGDB_OSGOQ_EXPORTS

LIBS    += -losgOQ -losgDB -losg -losgUtil $(GL_LIBS) $(OTHER_LIBS)

TARGET_BASENAME = osgOQ
include $(TOPDIR)/Make/cygwin_plugin_def
PLUGIN = $(PLUGIN_PREFIX)$(TARGET_BASENAME).$(PLUGIN_EXT)

include $(TOPDIR)/Make/makerules 
