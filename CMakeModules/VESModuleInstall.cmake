#This script requires these variables be defined:
# VES_TARGET_NAME       = The name of the library or application
# VES_TARGET_VERSION    = The version of the library or application
# VES_TARGET_EXPORT     = The export target tag
# VES_TARGET_CATEGORY   = This can be: Lib, App, Plugin, Test, Example
# VES_PUBLIC_HEADERS    = Headers to be installed
# VES_PRIVATE_HEADERS    = Headers to be installed
# VES_INCLUDE_DIRECTORY_NAME

set_target_properties( ${VES_TARGET_NAME} PROPERTIES PROJECT_LABEL "${VES_TARGET_CATEGORY} ${VES_TARGET_NAME}" )

if(${VES_TARGET_CATEGORY} STREQUAL "App")
    install(TARGETS ${VES_TARGET_NAME}
        RUNTIME DESTINATION ${VES_INSTALL_BINDIR} COMPONENT runtime)
elseif(${VES_TARGET_CATEGORY} STREQUAL "Test" OR ${VES_TARGET_CATEGORY} STREQUAL "Example")
    install(TARGETS ${VES_TARGET_NAME}
        RUNTIME DESTINATION share/${PROJECT_NAME}/bin COMPONENT runtime)
elseif(${VES_TARGET_CATEGORY} STREQUAL "Lib" OR ${VES_TARGET_CATEGORY} STREQUAL "Plugin")
    set_target_properties( ${VES_TARGET_NAME} PROPERTIES VERSION ${VES_TARGET_VERSION} )
    set_target_properties( ${VES_TARGET_NAME} PROPERTIES SOVERSION ${VES_TARGET_VERSION} )

    source_group(
        "Header Files"
        FILES ${VES_PRIVATE_HEADERS})

    install(TARGETS ${VES_TARGET_NAME}
        EXPORT ${VES_TARGET_EXPORT}
        RUNTIME DESTINATION ${VES_INSTALL_BINDIR} COMPONENT runtime
        LIBRARY DESTINATION ${VES_INSTALL_LIBDIR} COMPONENT runtime
        ARCHIVE DESTINATION ${VES_INSTALL_ARCHIVEDIR} COMPONENT development)

    if(NOT DEFINED VES_INCLUDE_DIRECTORY_NAME)
        set(VES_INCLUDE_DIRECTORY_NAME "${VES_TARGET_NAME}")
    endif(NOT DEFINED VES_INCLUDE_DIRECTORY_NAME)

    install(FILES ${VES_PUBLIC_HEADERS}
        DESTINATION ${VES_INSTALL_INCDIR}/${VES_INCLUDE_DIRECTORY_NAME}
        COMPONENT development)
endif()
