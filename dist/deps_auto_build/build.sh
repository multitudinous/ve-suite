#!/bin/bash

#
# Define the platform
# 
PLATFORM=`uname -s`
#http://en.wikipedia.org/wiki/Uname
case $PLATFORM in
  CYGWIN*)
    # If the call came via symlink, then use its target instead
    arg=$0; [[ -L $0 ]] && arg=$( stat -f '%Y' "$0" )
    SCRIPTDIR=$( 2>/dev/null cd "${arg%/*}" >&2; echo "`pwd -P`/${arg##*/}" )
    SCRIPTDIR=$( dirname "$SCRIPTDIR" )
    PLATFORM=Windows;
    HOME=$USERPROFILE;
    # Does cmake exist?
    type -P cmake &>/dev/null || { echo "CMake is not installed." >&2; exit 1; }
    # Just going to assume VS 9 for now
    CMAKE_GENERATOR="Visual Studio 9 2008"
    REGPATH="/proc/registry/HKEY_LOCAL_MACHINE/SOFTWARE"
    ;;
  Darwin | Linux)
    ;;
  *)
    echo "Unrecognized OS: $PLATFORM" >&2
    kill -SIGINT $$
    ;;
esac
export PLATFORM

#
# Define the architecture
#
if [ $PLATFORM = "Darwin" ]; then
  ARCH=64-bit;
else
  [ -z "${ARCH}" ] && ARCH=`uname -m`
fi

#http://en.wikipedia.org/wiki/Uname
case $ARCH in
  i[3-6]86 | x86 | 32-bit)
    ARCH=32-bit
    ;;
  x86_64 | x64 | 64-bit)
    ARCH=64-bit
    if [ $PLATFORM = "Windows" ]; then
      REGPATH=${REGPATH}/WOW6432Node
      CMAKE_GENERATOR="${CMAKE_GENERATOR} Win64"
    fi
    ;;
  *)
    echo "Unrecognized Architecture: $ARCH" >&2
    kill -SIGINT $$
    ;;
esac
export ARCH

#
# Some Windows-only variables
#
if [ $PLATFORM = "Windows" ]; then
  declare -a MSVC_REGPATH=( "${REGPATH}"/Microsoft/VisualStudio/SxS/VC7/* )
  VCInstallDir=$( awk '{ print }' "${MSVC_REGPATH[@]: -1}" )

  declare DOTNET_REGVAL=( "${REGPATH}"/Microsoft/.NETFramework/InstallRoot )
  # .NET version is hardcoded to 3.5 for now
  DotNETInstallDir=$( awk '{ gsub( "", "" ); print }' "${DOTNET_REGVAL}" )v3.5

  #declare -a CMAKE_REGPATH=( "${REGPATH}"/Kitware/* )
  #CMAKEInstallDir=$( awk '{ print }' "${CMAKE_REGPATH[@]: -1}"/@ )

  #export Path="${DotNETInstallDir}";${Path}
  MSBUILD="${DotNETInstallDir}/MSBuild.exe"
fi

#
# DEV_BASE_DIR defines the base directory for all development packages.
# May be overriden with a shell variable
[ -z "${DEV_BASE_DIR}" ] && export DEV_BASE_DIR=${HOME}/dev/deps

#
# Some useful global variables
#
export OSG_DIR=${DEV_BASE_DIR}/osg_2.8.3/install-64-bit
export OSGWORKS_ROOT=${DEV_BASE_DIR}/osgWorks/install-64-bit
export BULLET_ROOT=${DEV_BASE_DIR}/bullet-2.77/install-64-bit
export OSGBULLET_ROOT=${DEV_BASE_DIR}/osgBullet/install-64-bit
export OSGEPHEMERIS_ROOT=${DEV_BASE_DIR}/osgEphemeris/install-64-bit
export BOOST_INSTALL_DIR=${DEV_BASE_DIR}/bullet-2.77/install-64-bit
export BOOST_INSTALL_DIR=/opt/local
export CTAGS_INSTALL_DIR=/opt/local
export TAGS_DIR=${HOME}/.vim/tags

#
# some "over-writeable" variables
#
CMAKE=cmake
CONFIGURE=./configure
SCONS=scons
MAKE=make
BJAM=bjam

function bye()
{
  [ $# -eq 0 ] && usage || echo Exiting: $1
  exit 1
}

function usage()
{
echo "
  usage: $0 [ options ] <package> ....

  This function builds the named package.

  OPTIONS:
    -h      Show this message
    -k      Check out the source code
    -u      Update the source code
    -c      Clean the build directory
    -p      Execute prebuild script, e.g., cmake, configure, and autogen
    -b      Build
    -j      Build with multithreading enabled
            Requires argument to specify number of jobs (1:8) to use
    -U      Subversion username to use for private repo
    -d      Create disk image containing install files for package
    -t      Create tag file with exuberant ctags" >&2
}

function ctags()
{
  ${CTAGS_INSTALL_DIR}/bin/ctags -RI --c++-kinds=+p --fields=+iaS --extra=+q --languages=c++ .
  [ -d "${TAGS_DIR}" ] || mkdir -p "${TAGS_DIR}"
  rm ${TAGS_DIR}/${1}
  mv tags ${TAGS_DIR}/${1}
}

function source_retrieval()
{
  case ${SOURCE_RETRIEVAL_METHOD} in
    svn)
      cd "${DEV_BASE_DIR}";
      svn co ${SOURCE_URL} "${BASE_DIR}";
      ;;
    private-svn)
      cd "${DEV_BASE_DIR}";
      svn co ${SOURCE_URL} "${BASE_DIR}" --username="${SVN_USERNAME}";
      ;;
    wget)
      [ -z "${SOURCE_FORMAT}" ] && ( echo "SOURCE_FORMAT undefined in package $package"; return; )
      cd "${DEV_BASE_DIR}";
      if [ -d "${BASE_DIR}" ]; then
        echo "We have already downloaded $package"; 
        return;
      fi
      wget ${SOURCE_URL}
      case ${SOURCE_FORMAT} in
        tgz)
          tar xvfz `basename ${SOURCE_URL}`;
          rm -f `basename ${SOURCE_URL}`;
          ;;
        bz2)
          tar xvfj `basename ${SOURCE_URL}`;
          rm -f `basename ${SOURCE_URL}`;
          ;;
        *)
          echo "Source format ${SOURCE_FORMAT} not supported";
          ;;
      esac
      ;;
    *)
      echo "Source retrieval method ${SOURCE_RETRIEVAL_METHOD} not supported";
      ;;
  esac
}

function e()
{
  package=$1

  #is this option really a package
  if [ ! -e $package ]; then
    echo "Ain't no package $package";
    return;
  fi

  #reset the var controlling wether to install an fpc file
  SKIP_FPC_INSTALL="yes"
  
  #setup the build types unless other wise specified in a build file
  case $PLATFORM in
    Windows)
      BUILD_METHOD=msbuild
      ;;
    Darwin)
      BUILD_METHOD=make
      ;;
    Linux)
      ;;
   esac

  #setup the package specific vars
  . $package

  #check to make sure that the base dir is defined for the package
  [ -z "${BASE_DIR}" ] && ( echo "BASE_DIR undefined in package $package"; return; )

  #checkout the source for download the source
  if [ "${check_out_source}" = "yes" ]; then
    [ -z "${SOURCE_URL}" ] && ( echo "SOURCE_URL undefined in package $package"; return; )
    [ -z "${SOURCE_RETRIEVAL_METHOD}" ] && ( echo "SOURCE_RETRIEVAL_METHOD undefined in package $package"; return; )
    source_retrieval;
  fi

  #update the source if needed
  if [ "${update_source}" = "yes" ]; then
    [ -z "${SOURCE_RETRIEVAL_METHOD}" ] && \
    ( echo "SOURCE_RETRIEVAL_METHOD undefined in package $package"; return; )
    case ${SOURCE_RETRIEVAL_METHOD} in
      svn | private-svn)
        if [ -d "${BASE_DIR}" ]; then
          cd "${BASE_DIR}";
          svn up;

        # Assume that if the base directory does not exist, it has not been checked out
        # test and perform a checkout
        else
          echo "${BASE_DIR} non-existent, checking out ...."
          [ -z "${SOURCE_URL}" ] && ( echo "SOURCE_URL undefined in package $package"; return; )
          [ -z "${SOURCE_RETRIEVAL_METHOD}" ] && ( echo "SOURCE_RETRIEVAL_METHOD undefined in package $package"; return; )

          source_retrieval;
        fi
        ;;
      *)
        echo "Source retrieval method ${SOURCE_RETRIEVAL_METHOD} not supported";
        ;;
    esac
  fi

  #prebuild for the package
  if [ "${prebuild}" = "yes" ] && [ "${SKIP_PREBUILD}" != "yes" ]; then
    [ -z "${BUILD_DIR}" ] && (echo "BUILD_DIR undefined in package $package"; return)
    [ -z "${PREBUILD_METHOD}" ] && (echo "PREBUILD_METHOD undefined in package $package"; return)
    [ -z "${SOURCE_DIR}" ] && (echo "SOURCE_DIR undefined in package $package"; return)
    [ -d "${BUILD_DIR}" ] || mkdir -p "${BUILD_DIR}"
    case ${PREBUILD_METHOD} in
      cmake)
        cd "${BUILD_DIR}";
        ${CMAKE} "${SOURCE_DIR}" "${CMAKE_PARAMS[@]}";
        ;;
      configure)
        cd "${BUILD_DIR}";
        ${CONFIGURE} "${CONFIGURE_PARAMS}";
        ;;
      bjam)
        cd "${SOURCE_DIR}";
        "${BJAM_PREBUILD}";
        ;;
      *)
        echo "Pre-Build method ${PREBUILD_METHOD} unsupported";
        ;;
    esac
  fi

  #build the package
  if [ "${build}" = "yes" ]; then
    [ -z "${BUILD_DIR}" ] && ( echo "BUILD_DIR undefined in package $package"; return; )
    [ -z "${BUILD_METHOD}" ] && ( echo "BUILD_METHOD undefined in package $package"; return; )
    [ -d "${BUILD_DIR}" ] || mkdir -p "${BUILD_DIR}"
    [ -z "$multithreading_jobs" ] || JCMD="-j $multithreading_jobs" || MCMD='/p:MultiProcessorCompilation=true /m:"$multithreading_jobs" /p:BuildInParallel=false'
    case ${BUILD_METHOD} in
      msbuild)
        cd "${BUILD_DIR}";

        for name in "${MSVC_PROJECT_NAMES[@]}"; do
          PROJ_STR="$PROJ_STR$name$( [ "$name" != "${MSVC_PROJECT_NAMES[@]: -1}" ] && echo ';' )";
        done

        "${MSBUILD}" "$MSVC_SOLUTION" /t:"$PROJ_STR" "$MCMD"
         /p:Configuration="$MSVC_CONFIG" /p:Platform="$MSVC_PLATFORM" \
         /p:TargetFrameworkVersion=v3.5 /p:ToolsVersion=2.0 \
         /verbosity:Detailed /p:WarningLevel=1;
         #/p:BuildProjectReferences=false
        ;;
      make)
        cd "${BUILD_DIR}";
        ${MAKE} ${JCMD} ${BUILD_TARGET};
        ;;
      scons)
        cd "${BUILD_DIR}";
        ${SCONS} ${BUILD_TARGET} ${JCMD} ${SCONS_PARAMS};
        ;;
      bjam)
        cd "${SOURCE_DIR}";
        ${BJAM} ${BJAM_PARAMS} ${BUILD_TARGET} ${JCMD};
        ;;
      *)
        echo "Build method ${BUILD_METHOD} unsupported"
        ;;
    esac
    if [ "${SKIP_FPC_INSTALL}" != "yes" ]; then
      [ -d "${INSTALL_DIR}/lib/flagpoll" ] || mkdir -p "${INSTALL_DIR}/lib/flagpoll"
      cp ${VES_SRC_DIR}/dist/linux/fpc_deps_files/${FPC_FILE}.in ${INSTALL_DIR}/lib/flagpoll/${FPC_FILE};
    fi
  fi

  #clean the build
  if [ "${clean_build_dir}" = "yes" ]; then
    [ -z "${BUILD_DIR}" ] && ( echo "BUILD_DIR undefined in package $package"; return; )
    [ -d "${BUILD_DIR}" ] || ( echo "${BUILD_DIR} non existent."; return; )
    [ -z "$multithreading_jobs" ] || JCMD="-j $multithreading_jobs"
    case ${BUILD_METHOD} in
      msbuild)
        cd "${BUILD_DIR}";

        for name in "${MSVC_PROJECT_NAMES[@]}"; do
          PROJ_STR="$PROJ_STR${name}:Clean$( [ "$name" != "${MSVC_PROJECT_NAMES[@]: -1}" ] && echo ';' )";
        done

        "${MSBUILD}" "$MSVC_SOLUTION" /t:"$PROJ_STR" \
         /p:Configuration="$MSVC_CONFIG" /p:Platform="$MSVC_PLATFORM" \
         /m:"$multithreading_jobs" /p:BuildInParallel=false \
         /verbosity:Normal /p:WarningLevel=0;
        ;;
      make)
        cd "${BUILD_DIR}";
        ${MAKE} clean;
        ;;
      *)
        echo "Build method ${BUILD_METHOD} unsupported";
        ;;
    esac
  fi
  
  #Build the ctag files
  if [ "${build_ctag_files}" = "yes" ]; then
    cd ${INSTALL_DIR}/include; 
    ctags $package;
  fi
  
  #Build the dmg file
  if [ "${build_dmg_installer}" = "yes" ]; then
    echo "DMG package installer not working yet for $package";
  fi
}

while getopts "hkucpbj:U:dt" opts
do
case $opts in
  h)
    usage
    kill -SIGINT $$
    ;;
  k) export check_out_source="yes" ;;
  u) export update_source="yes" ;;
  c) export clean_build_dir="yes";;
  p) export prebuild="yes";;
  b) export build="yes";;
  j)
    if [[ $OPTARG =~ [^1-8] ]] ; then
      echo "Error: '$OPTARG' not a valid number." >&2;
      usage;
      kill -SIGINT $$;
    fi
    export multithreading_jobs=$OPTARG
    export build="yes"  # implied
    ;;
  U)
    export SVN_USERNAME=$OPTARG
    ;;
  t)
    export build_ctag_files="yes";;
  d)
    export build_dmg_installer="yes";;
  ?)
    echo "Invalid option: $OPTARG" >&2
    usage
    kill -SIGINT $$
    ;;
  :)
    echo "Option $OPTARG requires an argument." >&2
    usage
    kill -SIGINT $$
    ;;
  *)
    usage
    ;;
esac
done
shift $(($OPTIND - 1))

[ -d "${DEV_BASE_DIR}" ] || mkdir -p "${DEV_BASE_DIR}"
[ $# -lt 1 ] && bye

echo -e "\nKernel: $PLATFORM $ARCH"
if [ $PLATFORM = "Windows" ]; then
  echo "VCInstallDir: $VCInstallDir"
  echo "CMAKEInstallDir: $CMAKEInstallDir"
fi
echo -e "DEV_BASE_DIR: ${DEV_BASE_DIR}\n"

for p in $@; do e $p; done

exit 0
