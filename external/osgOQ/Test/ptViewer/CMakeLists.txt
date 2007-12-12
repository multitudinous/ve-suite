SET(EXE_NAME ptViewer)

ADD_EXECUTABLE( ${EXE_NAME} 
    ptViewer.cpp
)

## On Windows, debug executables have a 'd' suffix.
IF(WIN32)
    SET_TARGET_PROPERTIES(${EXE_NAME} PROPERTIES DEBUG_POSTFIX d)
ENDIF(WIN32)

LINK_INTERNAL( ${EXE_NAME} 
    osgOQ
    osgText
    osgGA
    osgViewer
    osgUtil
    osgDB
    osg
    OpenThreads
)
