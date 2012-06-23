cmake_minimum_required (VERSION 2.6)

project( PropertySetBrowser )

SET( CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMakeModules" )
#SET( CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}/install-64-bit")
SET( Poco_DIR "${PROJECT_SOURCE_DIR}/CMakeModules" )
SET( POCO_ROOT "$ENV{POCO_INSTALL_DIR}" )
SET( Poco_INCLUDE_DIR "${POCO_ROOT}/include" )
SET( Poco_LIBRARY_DIR "${POCO_ROOT}/lib" )

#option(BUILD_EXAMPLES "Build examples?" ON)

SET(BOOST_INCLUDEDIR "$ENV{BOOST_INSTALL_DIR}/include" CACHE PATH "docstring")
SET(Persistence_ROOT "$ENV{Persistence_INSTALL_DIR}" CACHE PATH "docstring")
SET(EventManager_ROOT "$ENV{EventManager_INSTALL_DIR}" CACHE PATH "docstring")

FIND_PACKAGE(Boost)
FIND_PACKAGE(Poco COMPONENTS Foundation)
FIND_PACKAGE(Qt4 REQUIRED)


INCLUDE_DIRECTORIES( 
    ${Boost_INCLUDE_DIRS}
    ${Poco_INCLUDE_DIR}
    ${Persistence_ROOT}/include
    ${EventManager_ROOT}/include
    ${PROJECT_SOURCE_DIR}/qtpropertybrowser-2.5_1-opensource/src
)

LINK_DIRECTORIES(${Poco_LIBRARY_DIRS} ${Persistence_ROOT}/lib64 ${EventManager_ROOT}/lib64)

# Alias for the qtpropertybrowser path
SET( QTPBDIR ${CMAKE_SOURCE_DIR}/src/qtpropertybrowser-2.5_1-opensource/src )

################################################################################
# Platform specific definitions
IF(WIN32)
    IF(MSVC)
        # To enable /MP, parralel build on MSVC
        OPTION(WIN32_USE_MP "Set to OFF to diable /MP." ON)
        MARK_AS_ADVANCED(WIN32_USE_MP)
        IF(WIN32_USE_MP)
            SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MP")
        ENDIF(WIN32_USE_MP)

    ENDIF(MSVC)
ENDIF (WIN32)

IF(UNIX AND NOT WIN32 AND NOT APPLE)
  IF(CMAKE_SIZEOF_VOID_P MATCHES "8")
      SET(LIB_POSTFIX "64" CACHE STRING "suffix for 32/64 dir placement")
      MARK_AS_ADVANCED(LIB_POSTFIX)
  ENDIF()
ENDIF()
IF(NOT DEFINED LIB_POSTFIX)
    SET(LIB_POSTFIX "")
ENDIF()

################################################################################
# Create lib directory if required

IF("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")
   FILE(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
ENDIF("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_BINARY_DIR}")

################################################################################

SET(OUTPUT_LIBDIR ${PROJECT_BINARY_DIR}/lib)
MAKE_DIRECTORY(${OUTPUT_LIBDIR})
SET(LIBRARY_OUTPUT_PATH ${OUTPUT_LIBDIR})


add_subdirectory(src)

