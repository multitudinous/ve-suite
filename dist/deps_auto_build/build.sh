#!/bin/bash

#
# Define the platform
#
PLATFORM=`uname -s`
#http://en.wikipedia.org/wiki/Uname
case $PLATFORM in
  CYGWIN*)
    PLATFORM=Windows;
    HOME=$USERPROFILE;
    ;;
  Darwin)
    ;;
  Linux)
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
[ -z "${ARCH}" ] && ARCH=`uname -m`
#http://en.wikipedia.org/wiki/Uname
case $ARCH in
  i[3-6]86)
    ARCH=32-bit
    ;;
  x86)
    ARCH=32-bit
    ;;
  32-bit)
    ;;
  x86_64)
    ARCH=64-bit
    ;;
  x64)
    ARCH=64-bit
    ;;
  64-bit)
    ;;
  *)
    echo "Unrecognized Architecture: $ARCH" >&2
    kill -SIGINT $$
    ;;
esac
export ARCH

#
# DEV_BASE_DIR defines the base directory for all development packages.
# May be overriden with a shell variable
[ -z "${DEV_BASE_DIR}" ] && export DEV_BASE_DIR=${HOME}/dev/deps

echo "
  Kernel: $PLATFORM $ARCH
  DEV_BASE_DIR: ${DEV_BASE_DIR}
"

#
# Some useful global variables
#
export OSG_DIR=${DEV_BASE_DIR}/OpenSceneGraph-2.8.3/osg_2.8.3_install
export OSGWORKS_ROOT=${OSG_DIR}/include
export BULLET_ROOT=${DEV_BASE_DIR}/bullet-2.77/install

#
# some "over-writeable" variables
#
CMAKE=cmake
CONFIGURE=./configure
SCONS=scons
MAKE=make
if [ $PLATFORM = "Windows" ]; then
  MAKE=nmake;
  case $ARCH in
    32-bit)
      #HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\VisualStudio\SxS\VS7
      ;;
    64-bit)
      #HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\VisualStudio\SxS\VS7
      ;;
  esac
fi

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
      -d      Create disk image containing install files for package
      -t      Create tag file with exuberant ctags" >&2
}

function e()
{
  package=$1

  if [ ! -e $package ]; then
    echo "Ain't no package $package";
    return;
  fi

  . $package

  [ -z "${BASE_DIR}" ] && ( echo "BASE_DIR undefined in package $package"; return; )

  if [ "${check_out_source}" = "yes" ]; then
    [ -z "${SOURCE_URL}" ] && ( echo "SOURCE_URL undefined in package $package"; return; )
    [ -z "${SOURCE_RETRIEVAL_METHOD}" ] && ( echo "SOURCE_RETRIEVAL_METHOD undefined in package $package"; return; )

    case ${SOURCE_RETRIEVAL_METHOD} in
      svn)
        cd "${DEV_BASE_DIR}";
        svn co ${SOURCE_URL} "${BASE_DIR}";
        ;;
      wget)
        [ -z "${SOURCE_FORMAT}" ] && ( echo "SOURCE_FORMAT undefined in package $package"; return; )
        cd "${DEV_BASE_DIR}";
        wget ${SOURCE_URL}
        case ${SOURCE_FORMAT} in
          tgz)
            tar xvfz `basename ${SOURCE_URL}`;
            rm -f `basename ${SOURCE_URL}`;
            ;;
          bz2)
            tar xvfj `basename ${SOURCE_URL}`;
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
  fi

  if [ "${update_source}" = "yes" ]; then
    [ -z "${SOURCE_RETRIEVAL_METHOD}" ] && \
    ( echo "SOURCE_RETRIEVAL_METHOD undefined in package $package"; return; )
    case ${SOURCE_RETRIEVAL_METHOD} in
      svn)
        if [ -d "${BASE_DIR}" ]; then
          cd "${BASE_DIR}";
          svn up;

        # Assume that if the base directory does not exist, it has not been checked out
        # test and perform a checkout
        else
          echo "${BASE_DIR} non-existent, checking out ...."
          [ -z "${SOURCE_URL}" ] && ( echo "SOURCE_URL undefined in package $package"; return; )
          [ -z "${SOURCE_RETRIEVAL_METHOD}" ] && ( echo "SOURCE_RETRIEVAL_METHOD undefined in package $package"; return; )

          case ${SOURCE_RETRIEVAL_METHOD} in
            svn)
              cd "${DEV_BASE_DIR}";
              svn co ${SOURCE_URL};
              ;;
            wget)
              [ -z "${SOURCE_FORMAT}" ] && (echo "SOURCE_FORMAT undefined in package $package"; return)
              cd "${DEV_BASE_DIR}";
              wget ${SOURCE_URL};
              case ${SOURCE_FORMAT} in
                tgz)
                  tar xvfz `basename ${SOURCE_URL}`;
                  rm -f `basename ${SOURCE_URL}`;
                  ;;
                *)
                  echo "Source format ${SOURCE_FORMAT} not supported";
                  ;;
              esac
              ;;
            *)
              echo Source retrieval method ${SOURCE_RETRIEVAL_METHOD} not supported;
              ;;
          esac
        fi
        ;;
      *)
        echo Source retrieval method ${SOURCE_RETRIEVAL_METHOD} not supported;
        ;;
    esac
  fi

  if [ "${prebuild}" = "yes" ] && [ "${SKIP_PREBUILD}" != "yes" ]; then
    [ -z "${BUILD_DIR}" ] && (echo "BUILD_DIR undefined in package $package"; return)
    [ -z "${BUILD_METHOD}" ] && (echo "BUILD_METHOD undefined in package $package"; return)
    [ -z "${SOURCE_DIR}" ] && (echo "SOURCE_DIR undefined in package $package"; return)
    [ -d "${BUILD_DIR}" ] || mkdir -p "${BUILD_DIR}"
    case ${BUILD_METHOD} in
      cmake)
        cd "${BUILD_DIR}";
        ${CMAKE} ${CMAKE_PARAMS} "${SOURCE_DIR}";
        ;;
      autotools)
        cd "${BUILD_DIR}";
        ${CONFIGURE} ${CONFIGURE_PARAMS};
        ;;
      *)
        echo "Build method ${BUILD_METHOD} unsupported";
        ;;
    esac
  fi

  if [ "${build}" = "yes" ]; then
    [ -z "${BUILD_DIR}" ] && ( echo "BUILD_DIR undefined in package $package"; return; )
    [ -z "${BUILD_METHOD}" ] && ( echo "BUILD_METHOD undefined in package $package"; return; )
    [ -d "${BUILD_DIR}" ] || mkdir -p "${BUILD_DIR}"
    [ -z "$multithreading_jobs" ] || JCMD="-j $multithreading_jobs"
    case ${BUILD_METHOD} in
      cmake)
        cd "${BUILD_DIR}";
        ${MAKE} ${JCMD} ${BUILD_TARGET};
        ;;
      autotools)
        cd "${BUILD_DIR}";
        ${MAKE} ${JCMD} ${BUILD_TARGET};
        ;;
      scons)
        cd "${BUILD_DIR}";
        ${SCONS} ${BUILD_TARGET} ${JCMD} ${SCONS_PARAMS};
        ;;
      *)
        echo "Build method ${BUILD_METHOD} unsupported"
        ;;
    esac
  fi

  if [ "${clean_build_dir}" = "yes" ]; then
    [ -z "${BUILD_DIR}" ] && ( echo "BUILD_DIR undefined in package $package"; return; )
    [ -d "${BUILD_DIR}" ] || ( echo "${BUILD_DIR} non existent."; return; )
    case ${BUILD_METHOD} in
      cmake)
        cd "${BUILD_DIR}";
        ${MAKE} clean;
        ;;
      autotools)
        cd "${BUILD_DIR}";
        ${MAKE} clean;
        ;;
      *)
        echo "Build method ${BUILD_METHOD} unsupported";
        ;;
    esac
  fi
}

while getopts "hkucpbj:dt" opts
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
  #d)
    #args[5]="d"
    #;;
  #t)
    #args[6]="t"
    #;;
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

for p in $@; do e $p; done

exit 0
