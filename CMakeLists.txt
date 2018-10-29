cmake_minimum_required (VERSION 3.1.0)

project( ves )

SET( CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMakeModules" )

set(ves_MAJOR_VERSION 3)
set(ves_MINOR_VERSION 0)
set(ves_SUB_VERSION 0)
set(ves_VERSION ${ves_MAJOR_VERSION}.${ves_MINOR_VERSION}.${ves_SUB_VERSION})
set(VES_PACKAGE_VERSION ${ves_VERSION})

#Force the use of C++11
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 11)

#Set the include directory so that switchwire can be used as a sub project
if( NOT(${CMAKE_PROJECT_NAME} STREQUAL ${PROJECT_NAME}) )
    set(ves_INCLUDE_DIRS "${PROJECT_SOURCE_DIR}/src" PARENT_SCOPE)
    set(ves_LIBRARIES "TODO NEED Libs ves" PARENT_SCOPE)
endif()

include(FindOSGHelper)
find_package( OpenGL )

set( POCO_ROOT "Please specify" CACHE PATH "Root directory where POCO is installed" )
find_package(POCO 1.5.1 REQUIRED COMPONENTS PocoFoundation PocoUtil PocoXML PocoData PocoDataSQLite)

set( BOOST_ROOT "Please specify" CACHE PATH "Root directory where Boost is installed" )
set(BOOST_INCLUDEDIR "$ENV{BOOST_INSTALL_DIR}/include" CACHE PATH "docstring")
add_definitions( -DBOOST_ALL_NO_LIB )
add_definitions( -DBOOST_ALL_DYN_LINK )

set(Boost_USE_STATIC_LIBS       OFF)
set(Boost_USE_MULTITHREADED      ON)
set(Boost_USE_STATIC_RUNTIME    OFF)

find_package(Boost 1.53.0 REQUIRED COMPONENTS filesystem system program_options serialization date_time thread chrono regex)

find_package( crunchstore 0.2.0 REQUIRED )
if( crunchstore_FOUND )
    include( ${crunchstore_USE_FILE} )
endif(crunchstore_FOUND)

find_package( switchwire 0.2.0 REQUIRED)
if( switchwire_FOUND )
    include( ${switchwire_USE_FILE} )
endif(switchwire_FOUND)

find_package( propertystore 0.2.0 REQUIRED)
if( propertystore_FOUND )
    include( ${propertystore_USE_FILE} )
endif(propertystore_FOUND)

find_package( latticefx 0.0.1 REQUIRED)
if( latticefx_FOUND )
    include( ${latticefx_USE_FILE} )
endif(latticefx_FOUND)

find_package( backdropFX 0.1.0 REQUIRED)
if( backdropFX_FOUND )
    include( ${backdropFX_USE_FILE} )
endif(backdropFX_FOUND)

find_package( osgWorks 0.1.0 REQUIRED)
if( osgWorks_FOUND )
    include( ${osgWorks_USE_FILE} )
endif(osgWorks_FOUND)

find_package( storyteller 0.1.0 REQUIRED)
if( storyteller_FOUND )
    include( ${storyteller_USE_FILE} )
endif(storyteller_FOUND)

find_package( osgBullet 0.1.0 REQUIRED)
if( osgBullet_FOUND )
    include( ${osgBullet_USE_FILE} )
endif(osgBullet_FOUND)

include( FindBulletHelper )
if(BULLET_FOUND)
    #set( BULLET_LIBRARIES
    #    ${BULLET_COLLISION_LIBRARY}
    #    ${BULLET_MATH_LIBRARY}
    #    ${BULLET_DYNAMICS_LIBRARY}
    #    ${BULLET_SOFTBODY_LIBRARY})
    if( OSGBULLET_USE_DOUBLE_PRECISION )
        add_definitions( -DBT_USE_DOUBLE_PRECISION )
    endif()
endif(BULLET_FOUND)

find_package( osgEphemeris 0.1.0 REQUIRED)
if( osgEphemeris_FOUND )
    include( ${osgEphemeris_USE_FILE} )
endif(osgEphemeris_FOUND)

find_package( VTK 5.10.0 REQUIRED)
if( VTK_FOUND )
    include( ${VTK_USE_FILE} )
endif(VTK_FOUND)

set(Squirrel_ROOT "Please specify" CACHE PATH "Root directory where Squirrel is installed" )
find_package(Squirrel 3.0.0)

set(Sqrat_ROOT "Please specify" CACHE PATH "Root directory where Sqrat is installed" )
find_package(Sqrat 0.8)

find_package(Qt4 REQUIRED COMPONENTS QtCore QtGui QtOpenGL QtSvg QtXml)
include(${QT_USE_FILE})

set(XERCESC_ROOT_DIR "Please specify" CACHE PATH "Root directory where Xerces-C is installed" )
find_package(XercesC REQUIRED)

find_package(VRJuggler-OpenGL 3.0 REQUIRED)

find_package(X11)

include_directories( 
    ${Boost_INCLUDE_DIRS}
    ${POCO_INCLUDE_DIR}
    ${PROJECT_SOURCE_DIR}/src)

#if(Squirrel_FOUND AND Sqrat_FOUND)
#    INCLUDE_DIRECTORIES(
#        ${Sqrat_INCLUDE_DIR}
#        ${Squirrel_INCLUDE_DIR})
#endif(Squirrel_FOUND AND Sqrat_FOUND)

#option(BUILD_EXAMPLE "Build example?" ON)

include(VESSetupCompiler)

include(VESBinAndLibDirs)

#set(HAVE_SQUIRREL 0)
#if(Squirrel_FOUND AND Sqrat_FOUND)
#    set(HAVE_SQUIRREL 1)
#endif(Squirrel_FOUND AND Sqrat_FOUND)

add_subdirectory(src)
add_subdirectory(external)
add_subdirectory(share)

#
# Doxygen
#find_package( Doxygen )
#if( DOXYGEN_FOUND )
#    set( HAVE_DOT "NO" )
#    if( DOXYGEN_DOT_PATH )
#        set( HAVE_DOT "YES" )
#    endif()
#    
#    configure_file( ${PROJECT_SOURCE_DIR}/doc/doxyfile.cmake
#        ${PROJECT_BINARY_DIR}/doc/doxyfile
#    )
#    add_custom_target( Documentation ${DOXYGEN_EXECUTABLE}
#        ${PROJECT_BINARY_DIR}/doc/doxyfile
#    )
#    
#    add_subdirectory( doc/images )
#endif()

include(VESInstallPDBFiles)

set(VES_EXPORT_LIBRARY_TARGETS ves_open_xml ves_xplorer_eventmanager ves_xplorer_data ves_xplorer_scenegraph ves_xplorer_volume)
set(VES_CONFIGURE_VARS CMAKE_INSTALL_LIBDIR)

include(VESInstallAutoDiscoveryTools)

