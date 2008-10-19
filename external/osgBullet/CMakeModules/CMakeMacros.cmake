IF(WIN32)
    SET(CMAKE_DEBUG_POSTFIX d)
ENDIF(WIN32)

MACRO( SN_ADD_SHARED_LIBRARY TRGTNAME )
    ADD_LIBRARY( ${TRGTNAME} SHARED ${ARGN} )
    IF( WIN32 )
        SET_TARGET_PROPERTIES( ${TRGTNAME} PROPERTIES DEBUG_POSTFIX d )
    ENDIF( WIN32 )
ENDMACRO( SN_ADD_SHARED_LIBRARY TRGTNAME )

MACRO( SN_ADD_OSGPLUGIN TRGTNAME )
    ADD_LIBRARY( ${TRGTNAME} MODULE ${ARGN} )
    SET_TARGET_PROPERTIES( ${TRGTNAME} PROPERTIES PREFIX "" )
ENDMACRO( SN_ADD_OSGPLUGIN TRGTNAME )

MACRO( SN_ADD_EXECUTABLE TRGTNAME )
    ADD_EXECUTABLE( ${TRGTNAME} ${ARGN} )
    IF(WIN32)
        SET_TARGET_PROPERTIES( ${TRGTNAME} PROPERTIES DEBUG_POSTFIX d )
    ENDIF(WIN32)
ENDMACRO( SN_ADD_EXECUTABLE TRGTNAME )

MACRO( SN_LINK_LIBRARIES TRGTNAME)
    FOREACH( LINKLIB ${ARGN} )
        TARGET_LINK_LIBRARIES( ${TRGTNAME} optimized "${LINKLIB}" debug "${LINKLIB}${CMAKE_DEBUG_POSTFIX}" )
    ENDFOREACH( LINKLIB )
    TARGET_LINK_LIBRARIES( ${TRGTNAME} ${OPENGL_LIBRARIES} )
ENDMACRO( SN_LINK_LIBRARIES TRGTNAME)
