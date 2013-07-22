cmake_minimum_required (VERSION 2.8.8)

project( ves )

SET( CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMakeModules" )

set(ves_MAJOR_VERSION 3)
set(ves_MINOR_VERSION 0)
set(ves_SUB_VERSION 0)
set(ves_VERSION ${ves_MAJOR_VERSION}.${ves_MINOR_VERSION}.${ves_SUB_VERSION})
set(VES_PACKAGE_VERSION ${ves_VERSION})

#Set the include directory so that switchwire can be used as a sub project
if( NOT(${CMAKE_PROJECT_NAME} STREQUAL ${PROJECT_NAME}) )
    set(ves_INCLUDE_DIRS "${PROJECT_SOURCE_DIR}/src" PARENT_SCOPE)
    set(ves_LIBRARIES "TODO NEED Libs ves" PARENT_SCOPE)
endif()

include( FindOSGHelper )
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

find_package(Boost 1.53.0 REQUIRED COMPONENTS filesystem system program_options serialization date_time thread chrono)

find_package( crunchstore 0.2.0 )
if( crunchstore_FOUND )
    include( ${crunchstore_USE_FILE} )
endif(crunchstore_FOUND)

find_package( switchwire 0.2.0 )
if( switchwire_FOUND )
    include( ${switchwire_USE_FILE} )
endif(switchwire_FOUND)

find_package( propertystore 0.2.0 )
if( propertystore_FOUND )
    include( ${propertystore_USE_FILE} )
endif(propertystore_FOUND)

set( Squirrel_ROOT "Please specify" CACHE PATH "Root directory where Squirrel is installed" )
find_package(Squirrel 3.0.0)

set( Sqrat_ROOT "Please specify" CACHE PATH "Root directory where Sqrat is installed" )
find_package(Sqrat 0.8)

find_package(Qt4 REQUIRED COMPONENTS QtCore QtGui )
include(${QT_USE_FILE})

INCLUDE_DIRECTORIES( 
    ${Boost_INCLUDE_DIRS}
    ${POCO_INCLUDE_DIR}
)

if(Squirrel_FOUND AND Sqrat_FOUND)
    INCLUDE_DIRECTORIES( 
        ${Sqrat_INCLUDE_DIR}
        ${Squirrel_INCLUDE_DIR})
endif(Squirrel_FOUND AND Sqrat_FOUND)

option(BUILD_EXAMPLE "Build example?" ON)

include(VESSetupCompiler)

include(VESBinAndLibDirs)

set(HAVE_SQUIRREL 0)
if(Squirrel_FOUND AND Sqrat_FOUND)
    set(HAVE_SQUIRREL 1)
endif(Squirrel_FOUND AND Sqrat_FOUND)

add_subdirectory(src)

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

set(VES_EXPORT_LIBRARY_TARGETS ves)
set(VES_CONFIGURE_VARS CMAKE_INSTALL_LIBDIR)

include(VESInstallAutoDiscoveryTools)

set(CPACK_PACKAGE_VENDOR "Ames Lab")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A library for boosts signals and slots.")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.txt")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.txt") 
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
set(CPACK_RESOURCE_FILE_WELCOME "${CMAKE_CURRENT_SOURCE_DIR}/README.txt") 
set(CPACK_PACKAGE_VERSION_MAJOR "${switchwire_MAJOR_VERSION}")
set(CPACK_PACKAGE_VERSION_MINOR "${switchwire_MINOR_VERSION}")
set(CPACK_PACKAGE_VERSION_PATCH "${switchwire_SUB_VERSION}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "${PROJECT_NAME}_${VES_PACKAGE_VERSION}")
#This sits right below CPACK_PACKAGE_DEFAULT_LOCATION
set(CPACK_PACKAGING_INSTALL_PREFIX "/")
#set(CPACK_TOPLEVEL_TAG "${PROJECT_NAME}")
#set(CPACK_SET_DESTDIR TRUE)
if(WIN32 AND NOT UNIX)
  # There is a bug in NSI that does not handle full unix paths properly. Make
  # sure there is at least one set of four (4) backlasshes.
  set(CPACK_PACKAGE_ICON "${CMake_SOURCE_DIR}/Utilities/Release\\\\InstallIcon.bmp")
  set(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\\\MyExecutable.exe")
  set(CPACK_NSIS_DISPLAY_NAME "${CPACK_PACKAGE_INSTALL_DIRECTORY} My Famous Project")
  set(CPACK_NSIS_HELP_LINK "http:\\\\\\\\www.my-project-home-page.org")
  set(CPACK_NSIS_URL_INFO_ABOUT "http:\\\\\\\\www.my-personal-home-page.com")
  set(CPACK_NSIS_CONTACT "me@my-personal-home-page.com")
  set(CPACK_NSIS_MODIFY_PATH ON)
  set(CPACK_PACKAGE_EXECUTABLES "MyExecutable" "My Executable")
else(WIN32 AND NOT UNIX) 
  #This directory is the prefix to the default install prefix
  set(CPACK_PACKAGE_DEFAULT_LOCATION "/opt/${CPACK_PACKAGE_INSTALL_DIRECTORY}")
  set(CPACK_STRIP_FILES OFF)
  set(CPACK_SOURCE_STRIP_FILES OFF)
endif(WIN32 AND NOT UNIX)

include(CPack)
