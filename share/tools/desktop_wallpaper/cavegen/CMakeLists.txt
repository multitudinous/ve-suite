ADD_EXECUTABLE( cavegen
    cavegen.cpp
    RenderTexture.h
    RenderTexture.cpp
    Wall.h
    Wall.cpp
)

LINK_INTERNAL(
    cavegen
    osgViewer
    osgGA
    osgText
    osgDB
    osgUtil
    osg
    OpenThreads
)
