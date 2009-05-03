ADD_EXECUTABLE( cavepreview
    cavepreview.cpp
    ../cavegen/Wall.h
    ../cavegen/Wall.cpp
)

LINK_INTERNAL(
    cavepreview
    osgViewer
    osgGA
    osgText
    osgDB
    osgUtil
    osg
    OpenThreads
)
