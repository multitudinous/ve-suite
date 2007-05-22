TOPDIR = ../..
include $(TOPDIR)/Make/makedefs 

CXXFILES =\
	ptViewer.cpp\

LIBS     += -losgOQ -losgViewer -losgGA -losgUtil -losgDB -losg -losgText $(GL_LIBS) $(X_LIBS) $(OTHER_LIBS) 

INSTFILES = \
	$(CXXFILES)\
	GNUmakefile.inst=GNUmakefile

EXEC = ptViewer

INC      += $(X_INC)

include $(TOPDIR)/Make/makerules 

