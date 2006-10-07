set OS_NAME=`uname`
EXTRA_INCLUDES+= -I${CASROOT}/inc
EXTRA_LIBS+= -L${CASROOT}/${OS_NAME}/lib -lTKBRep -lTKGeomAlgo -lTKGeomBase -lTKMath 
EXTRA_CXXFLAGS+= -DCSFDB -DHAVE_CONFIG_H -DHAVE_WOK_CONFIG_H -DLIN -DLININTEL
DSO_PLUGIN_DEPS+= -L${CASROOT}/${OS_NAME}/lib -lxerces-c

#
