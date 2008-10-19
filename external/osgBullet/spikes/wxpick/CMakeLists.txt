IF ( WIN32 )
    FIND_PACKAGE( Boost 1.35.0 COMPONENTS filesystem date_time program_options regex system )
ELSE()
    FIND_PACKAGE( Boost 1.36 COMPONENTS filesystem date_time program_options regex system  )
ENDIF()
FIND_PACKAGE( wxWidgets REQUIRED base core gl )

SN_ADD_EXECUTABLE( spike_wxpick
                        wxboTreeControl.cpp wxboTreeControl.h
                        wxboUtil.cpp wxboUtil.h
                        wxpick.cpp wxpick.h
                        pickui.cpp pickui.h
    )
INCLUDE_DIRECTORIES( ${Boost_INCLUDE_DIRS} "./" )
LINK_DIRECTORIES( ${Boost_LIBRARY_DIRS} )

ADD_DEFINITIONS( ${wxWidgets_DEFINITIONS} )
SN_LINK_LIBRARIES( spike_wxpick ${wxWidgets_LIBRARIES} osg osgViewer osgDB osgGA )
