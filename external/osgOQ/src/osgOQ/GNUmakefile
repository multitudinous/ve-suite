TOPDIR = ../..
include $(TOPDIR)/Make/makedefs 

CXXFILES =\
	CustomRenderBin.cpp\
	OcclusionQueryContext.cpp\
	OcclusionQueryNode.cpp\
	OcclusionQueryRoot.cpp\
	OcclusionQueryVisitor.cpp\
	ConfigFileReader.cpp\

DEF     += -DOSGOQ_EXPORTS

LIBS    += -losgUtil -losgDB -losg $(GL_LIBS) $(OTHER_LIBS)

TARGET_BASENAME = osgOQ
LIB = $(LIB_PREFIX)$(TARGET_BASENAME).$(LIB_EXT)

include $(TOPDIR)/Make/makerules 
