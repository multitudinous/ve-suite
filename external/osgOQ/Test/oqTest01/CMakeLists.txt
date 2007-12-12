SET(EXE_NAME oqTest01)

ADD_EXECUTABLE( ${EXE_NAME} 
    oqTest01.cpp
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
