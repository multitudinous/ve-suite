cmake_minimum_required (VERSION 2.6)

project( Persistence )

SET( CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMakeModules" )
SET( Poco_DIR "${PROJECT_SOURCE_DIR}/CMakeModules" )
SET( POCO_ROOT "$ENV{POCO_INSTALL_DIR}" )
SET( Poco_INCLUDE_DIR "${POCO_ROOT}/include" )
SET( Poco_LIBRARY_DIR "${POCO_ROOT}/lib" )

# should we build in mongoDB support?
#option(USE_MONGO "Build mongoDB adapter?" ON)
option(BUILD_EXAMPLES "Build examples?" ON)

set(BOOST_INCLUDEDIR "$ENV{BOOST_INSTALL_DIR}/include" CACHE PATH "docstring")

FIND_PACKAGE(Boost)
FIND_PACKAGE(Poco COMPONENTS SQLite)

set(MONGOCLIENT_PATH "$ENV{MONGOCLIENT_INSTALL_DIR}" CACHE PATH "docstring")

INCLUDE_DIRECTORIES( 
    ${Boost_INCLUDE_DIRS}
    ${MONGOCLIENT_PATH}
    ${Poco_INCLUDE_DIR}
)

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

        # Other MSVC compilation flags
        #ADD_DEFINITIONS(-D_SCL_SECURE_NO_WARNINGS)
        #ADD_DEFINITIONS(-D_CRT_SECURE_NO_DEPRECATE)

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

