SN_ADD_EXECUTABLE( osgbpp osgbpp.cpp )
SN_LINK_LIBRARIES( osgbpp osgBullet osgDB osgGA osgUtil osgViewer OpenThreads ${COLLADA_LIBRARY} )
