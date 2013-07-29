cmake_minimum_required (VERSION 2.8.8)

project( gmtl )

set(gmtl_MAJOR_VERSION 0)
set(gmtl_MINOR_VERSION 7)
set(gmtl_SUB_VERSION 0)
set(gmtl_VERSION ${gmtl_MAJOR_VERSION}.${gmtl_MINOR_VERSION}.${gmtl_SUB_VERSION})
set(gmtl_PACKAGE_VERSION ${gmtl_VERSION})

#Set the include directory so that gmtl can be used as a sub project
if( NOT(${CMAKE_PROJECT_NAME} STREQUAL ${PROJECT_NAME}) )
    set(gmtl_INCLUDE_DIRS "${PROJECT_SOURCE_DIR}" PARENT_SCOPE)
endif()

#option(BUILD_EXAMPLE "Build example?" ON)

set(gmtl_INSTALL_BINDIR bin)

if(WIN32)
    set(gmtl_INSTALL_INCDIR include)
else()
    set(gmtl_INSTALL_INCDIR include/gmtl-${gmtl_PACKAGE_VERSION})
endif()

include_directories( ${PROJECT_SOURCE_DIR} )

############################################################
# Compile the gmtl header

file( GLOB_RECURSE gmtl_hdrs "gmtl/*" )

source_group( "Header Files" FILES ${gmtl_hdrs})

add_custom_target( HEADER_ONLY_TARGET SOURCES ${gmtl_hdrs} )

set_target_properties( HEADER_ONLY_TARGET PROPERTIES PROJECT_LABEL "GMTL Headers" )

set_target_properties( HEADER_ONLY_TARGET PROPERTIES LINKER_LANGUAGE CXX)

install(DIRECTORY ${PROJECT_SOURCE_DIR}/${PROJECT_NAME} 
        DESTINATION ${gmtl_INSTALL_INCDIR}
        FILES_MATCHING PATTERN "*.h")

############################################################

############################################################

#
# Create and install all of the auto find tools
# =============================================

# Export the package for use from the build-tree
# (this registers the build-tree with a global CMake-registry)
export(PACKAGE ${PROJECT_NAME})

# Create a gmtlBuildTreeSettings.cmake file for the use from the build tree
configure_file(${PROJECT_SOURCE_DIR}/packageBuildTreeSettings.cmake.in
    "${PROJECT_BINARY_DIR}/share/${PROJECT_NAME}/${PROJECT_NAME}BuildTreeSettings.cmake" @ONLY)
configure_file(${PROJECT_SOURCE_DIR}/packageConfigVersion.cmake.in
    "${PROJECT_BINARY_DIR}/share/${PROJECT_NAME}/${PROJECT_NAME}ConfigVersion.cmake" @ONLY)
configure_file(${PROJECT_SOURCE_DIR}/Usepackage.cmake.in
    "${PROJECT_BINARY_DIR}/share/${PROJECT_NAME}/Use${PROJECT_NAME}.cmake" @ONLY)

include(CMakePackageConfigHelpers)
# Create the gmtlConfig.cmake and gmtlConfigVersion files
configure_package_config_file( 
    ${PROJECT_SOURCE_DIR}/packageConfig.cmake.in 
        ${CMAKE_CURRENT_BINARY_DIR}/share/${PROJECT_NAME}/${PROJECT_NAME}Config.cmake
    INSTALL_DESTINATION share/${PROJECT_NAME}
    PATH_VARS gmtl_INSTALL_INCDIR)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/share/${PROJECT_NAME}/${PROJECT_NAME}Config.cmake 
              ${CMAKE_CURRENT_BINARY_DIR}/share/${PROJECT_NAME}/${PROJECT_NAME}ConfigVersion.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/share/${PROJECT_NAME}/Use${PROJECT_NAME}.cmake
        DESTINATION share/${PROJECT_NAME}
        COMPONENT development)

# Install the CMake find script for gmtl
configure_file(${PROJECT_SOURCE_DIR}/Findpackage.cmake.in
    "${PROJECT_BINARY_DIR}/Find${PROJECT_NAME}.cmake" @ONLY)
install(FILES "${PROJECT_BINARY_DIR}/Find${PROJECT_NAME}.cmake"
    DESTINATION "share/${PROJECT_NAME}/extra"
    COMPONENT development)

# Create flagpoll file
set(provides "${PROJECT_NAME}")
set(arch "noarch")
set(version "${gmtl_VERSION}")
set(prefix "\${fp_file_cwd}/../..")
set(exec_prefix "\${prefix}")
set(includedir "\${fp_file_cwd}/../../${gmtl_INSTALL_INCDIR}")

if(CMAKE_COMPILER_IS_GNUCXX OR ( CMAKE_CXX_COMPILER_ID STREQUAL "Clang" ) )
    set(gmtl_cxxflags "")
    set(include_path_flag "-I")
elseif(MSVC)
    set(include_path_flag "/I")
else()
    message(FATAL_ERROR "Compiler not recognized - can't generate values for Flagpoll file")
endif()

configure_file(gmtl.fpc.in "${CMAKE_CURRENT_BINARY_DIR}/gmtl.fpc" @ONLY)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/gmtl.fpc"
        DESTINATION "share/flagpoll"
        COMPONENT development)

install(FILES "${CMAKE_CURRENT_BINARY_DIR}/gmtl.pc"
        DESTINATION "lib/pkgconfig"
        COMPONENT development)

install(FILES gmtl-config
        DESTINATION ${gmtl_INSTALL_BINDIR}
        COMPONENT development)
