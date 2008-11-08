IF ( WIN32 )
    FIND_PACKAGE( Boost 1.35.0 COMPONENTS filesystem date_time program_options regex system )
ELSE()
    FIND_PACKAGE( Boost 1.36 COMPONENTS filesystem date_time program_options regex system  )
ENDIF()
FIND_PACKAGE( wxWidgets REQUIRED base core gl )

IF ( WIN32 )
# Add this to link flags.
# /SUBSYSTEM:WINDOWS
ENDIF()

SN_ADD_EXECUTABLE( spike_wxpick
    wxpick.cpp
    wxpick.h
)
INCLUDE_DIRECTORIES( ${Boost_INCLUDE_DIRS} "./" )
LINK_DIRECTORIES( ${Boost_LIBRARY_DIRS} )

ADD_DEFINITIONS( ${wxWidgets_DEFINITIONS} )
SN_LINK_LIBRARIES( spike_wxpick osgWxTree ${wxWidgets_LIBRARIES} osg osgViewer osgDB osgGA OpenThreads )
