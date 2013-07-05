cmake_minimum_required (VERSION 2.8.8)

project( squirrel )

set(squirrel_MAJOR_VERSION 3)
set(squirrel_MINOR_VERSION 0)
set(squirrel_SUB_VERSION 4)
set(squirrel_VERSION ${squirrel_MAJOR_VERSION}.${squirrel_MINOR_VERSION}.${squirrel_SUB_VERSION})
set(squirrel_PACKAGE_VERSION ${squirrel_VERSION})

#Set the include directory so that squirrel can be used as a sub project
if( NOT(${CMAKE_PROJECT_NAME} STREQUAL ${PROJECT_NAME}) )
    set(squirrel_INCLUDE_DIRS "${PROJECT_SOURCE_DIR}/src" PARENT_SCOPE)
    set(squirrel_LIBRARIES "squirrel" PARENT_SCOPE)
endif()

option(BUILD_EXAMPLE "Build example?" ON)

# Platform specific definitions
IF(WIN32)
    IF(MSVC)
        # To enable /MP, parralel build on MSVC
        OPTION(WIN32_USE_MP "Set to OFF to diable /MP." ON)
        MARK_AS_ADVANCED(WIN32_USE_MP)
        IF(WIN32_USE_MP)
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")
        ENDIF(WIN32_USE_MP)

        # Other MSVC compilation flags
        #ADD_DEFINITIONS(-D_SCL_SECURE_NO_WARNINGS)
        #ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE)

    ENDIF(MSVC)
    if(CMAKE_BUILD_TOOL MATCHES "(msdev|devenv|nmake)")
        add_definitions(/W2)
    endif()
ENDIF (WIN32)

if(CMAKE_COMPILER_IS_GNUCXX)
    include(GNUInstallDirs)
endif(CMAKE_COMPILER_IS_GNUCXX)

if(NOT DEFINED CMAKE_INSTALL_LIBDIR)
  set(CMAKE_INSTALL_LIBDIR lib)
endif(NOT DEFINED CMAKE_INSTALL_LIBDIR)

set(squirrel_INSTALL_INCDIR include)
set(squirrel_INSTALL_BINDIR bin)
set(squirrel_INSTALL_ARCHIVEDIR ${CMAKE_INSTALL_LIBDIR})
if(WIN32)
    SET(squirrel_INSTALL_LIBDIR bin)
else(WIN32)
    SET(squirrel_INSTALL_LIBDIR ${CMAKE_INSTALL_LIBDIR})
endif(WIN32)

# On windows provide the user with the pdb files 
# for debugging if they are present
if(MSVC)
    install(
        DIRECTORY ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/\${CMAKE_INSTALL_CONFIG_NAME}/
        DESTINATION ${CMAKE_INSTALL_LIBDIR}
        USE_SOURCE_PERMISSIONS
        COMPONENT development
        FILES_MATCHING PATTERN "*.pdb"
    )
endif(MSVC)

include_directories( ${PROJECT_SOURCE_DIR}/include )

############################################################
# Compile the squirrel library

file( GLOB squirrel_srcs "squirrel/*.cpp" )
file( GLOB squirrel_hdrs "include/*.h" )

list( APPEND squirrel_srcs ${squirrel_hdrs} )
add_library(squirrel ${squirrel_srcs})

set_target_properties( squirrel PROPERTIES PROJECT_LABEL "Lib Squirrel" )

set_target_properties( squirrel PROPERTIES VERSION ${squirrel_VERSION} )
if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    set_target_properties(squirrel PROPERTIES COMPILE_FLAGS "-fno-exceptions -fno-rtti -fno-strict-aliasing" )
endif(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")

source_group( "Header Files" FILES ${squirrel_hdrs})

install(TARGETS squirrel
    EXPORT squirrel-targets
    RUNTIME DESTINATION ${squirrel_INSTALL_BINDIR} COMPONENT runtime
    LIBRARY DESTINATION ${squirrel_INSTALL_LIBDIR} COMPONENT runtime
    ARCHIVE DESTINATION ${squirrel_INSTALL_ARCHIVEDIR} COMPONENT development)

install(FILES ${squirrel_hdrs}
    DESTINATION ${squirrel_INSTALL_INCDIR}/${VES_INCLUDE_DIRECTORY_NAME}
    COMPONENT development)
############################################################
# Compile the sqstdlib library

file( GLOB sqstdlib_srcs "sqstdlib/*.cpp" )

add_library(sqstdlib ${sqstdlib_srcs})

set_target_properties( sqstdlib PROPERTIES PROJECT_LABEL "Lib Sqstdlib" )

set_target_properties( sqstdlib PROPERTIES VERSION ${squirrel_VERSION} )
if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    set_target_properties(sqstdlib PROPERTIES COMPILE_FLAGS "-fno-exceptions -fno-rtti -fno-strict-aliasing" )
endif(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")

source_group( "Header Files" FILES ${sqstdlib_hdrs})

install(TARGETS sqstdlib
    EXPORT squirrel-targets
    RUNTIME DESTINATION ${squirrel_INSTALL_BINDIR} COMPONENT runtime
    LIBRARY DESTINATION ${squirrel_INSTALL_LIBDIR} COMPONENT runtime
    ARCHIVE DESTINATION ${squirrel_INSTALL_ARCHIVEDIR} COMPONENT development)

install(FILES ${sqstdlib_hdrs}
    DESTINATION ${squirrel_INSTALL_INCDIR}/${VES_INCLUDE_DIRECTORY_NAME}
    COMPONENT development)
############################################################


############################################################
# Compile the squirrel application
file( GLOB sq_srcs "sq/*.c" )

add_executable(sq ${sq_srcs})
target_link_libraries(sq sqstdlib squirrel)

install(TARGETS sq
    EXPORT squirrel-targets
    RUNTIME DESTINATION ${squirrel_INSTALL_BINDIR} COMPONENT runtime )

############################################################

#
# Create and install all of the auto find tools
# =============================================
set(VES_EXPORT_LIBRARY_TARGETS squirrel sqstdlib)
# Add all targets to the build-tree export set
if( EXISTS "${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}LibraryDepends.cmake" )
    export(TARGETS squirrel sqstdlib APPEND
        FILE "${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}LibraryDepends.cmake")
else()
    export(TARGETS squirrel sqstdlib
        FILE "${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}LibraryDepends.cmake")
endif()

# Export the package for use from the build-tree
# (this registers the build-tree with a global CMake-registry)
export(PACKAGE ${PROJECT_NAME})

# Create a squirrelBuildTreeSettings.cmake file for the use from the build tree
configure_file(${PROJECT_SOURCE_DIR}/packageBuildTreeSettings.cmake.in
    "${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}BuildTreeSettings.cmake" @ONLY)
configure_file(${PROJECT_SOURCE_DIR}/packageConfigVersion.cmake.in
    "${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}ConfigVersion.cmake" @ONLY)
configure_file(${PROJECT_SOURCE_DIR}/Usepackage.cmake.in
    "${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/Use${PROJECT_NAME}.cmake" @ONLY)

include(CMakePackageConfigHelpers)
# Create the squirrelConfig.cmake and squirrelConfigVersion files
configure_package_config_file( 
    ${PROJECT_SOURCE_DIR}/packageConfig.cmake.in 
        ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}Config.cmake
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PATH_VARS CMAKE_INSTALL_LIBDIR)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}Config.cmake 
              ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}ConfigVersion.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR}/Use${PROJECT_NAME}.cmake
        DESTINATION ${CMAKE_INSTALL_LIBDIR}
        COMPONENT development)

# Install the CMake find script for storyteller
configure_file(${PROJECT_SOURCE_DIR}/Findpackage.cmake.in
    "${PROJECT_BINARY_DIR}/Find${PROJECT_NAME}.cmake" @ONLY)
install(FILES "${PROJECT_BINARY_DIR}/Find${PROJECT_NAME}.cmake"
    DESTINATION "share/${PROJECT_NAME}/extra"
    COMPONENT development)
    #RENAME Find${PROJECT_NAME}.cmake)

# Install the export set for use with the install-tree
install(EXPORT ${PROJECT_NAME}-targets DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT development)

