#!/bin/bash

if [ -z "${VES_SRC_DIR}" ]; then
  export VES_SRC_DIR=$PWD/../../
fi

#
# Define the platform
#
function platform()
{
  PLATFORM=`uname -s`
  #http://en.wikipedia.org/wiki/Uname
  case $PLATFORM in
    CYGWIN*)
      #Test for 64-buit capability
      if [[ "${PLATFORM}" = *WOW64 ]]; then
        ARCH=64-bit
        if [[ "${1}" = 32 ]]; then
          echo "Building 32-bit on x64."
          ARCH=32-bit
        else
          echo "Building 64-bit on x64"
        fi
      else
        echo "Building 32-bit on x86"
        ARCH=32-bit
      fi
      PLATFORM=Windows;
      HOME=$USERPROFILE;
      # Does cmake exist?
      type -P cmake &>/dev/null || { echo "CMake is not installed." >&2; kill -SIGINT $$; }
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
  export ARCH
}

#
# Define the architecture
#
function arch()
{
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
        # export REGPATH=${REGPATH}/Wow6432Node
        export CMAKE_GENERATOR="${CMAKE_GENERATOR} Win64"
      fi
      CMAKE_PARAMS+=( -G "${CMAKE_GENERATOR}" )
      ;;
    *)
      echo "Unrecognized Architecture: $ARCH" >&2
      kill -SIGINT $$
      ;;
  esac
  export ARCH
}

#
# Some Windows-only variables
#
function windows()
{
  if [ $PLATFORM = "Windows" ]; then
    declare -a MSVC_REGPATH=( "${REGPATH}"/Microsoft/VisualStudio/SxS/V*7 )
    #VCInstallDir=$( awk '{ print }' "${MSVC_REGPATH[1]}" )
    #VSInstallDir=$( awk '{ print }' "${MSVC_REGPATH[@]: -1}" )

    VS_REGPATH=( "${REGPATH}"/Microsoft/VisualStudio/9.0/InstallDir )
    VSInstallDir=$( awk '{ print }' "${VS_REGPATH}" )

    declare DOTNET_REGVAL=( "${REGPATH}/Microsoft/.NETFramework/InstallRoot" )
    # .NET version is hardcoded to 3.5 for now
    DotNETInstallDir=$( awk '{ gsub( "", "" ); print }' "${DOTNET_REGVAL}" )v3.5

    #declare -a CMAKE_REGPATH=( "${REGPATH}"/Kitware/* )
    #CMAKEInstallDir=$( awk '{ print }' "${CMAKE_REGPATH[@]: -1}/@" )

    #export Path="${DotNETInstallDir}";${Path}
    MSBUILD="${DotNETInstallDir}/MSBuild.exe"
    DEVENV="devenv.com"
    declare -a PYTHON_REGPATH=( "${REGPATH}"/Python/PythonCore/* )
    export PYTHONHOME=$( awk '{ print }' "${PYTHON_REGPATH[0]}/InstallPath/@" )
    export PYTHONPATH=$( awk '{ print }' "${PYTHON_REGPATH[0]}/PythonPath/@" )
    #DRIVE_LETTER="${PYTHONHOME:0:1}"
    echo "Using Python $PYTHONHOME"
    echo "Using Python Path $PYTHONPATH"
    export PATH=$PYTHONHOME/Scripts:$PYTHONHOME:${VSInstallDir}:$PATH
    echo ${VSInstallDir}
  fi
}

#
# DEV_BASE_DIR defines the base directory for all development packages.
# May be overriden with a shell variable
#
[ -z "${DEV_BASE_DIR}" ] && export DEV_BASE_DIR=${HOME}/dev/deps

#
# Some useful global variables
#
export CTAGS_INSTALL_DIR=/opt/local
export TAGS_DIR=${HOME}/.vim/tags
export OSG_INSTALL_DIR=${DEV_BASE_DIR}/osg_2.8.3/install-64-bit
export BOOST_INSTALL_DIR=${DEV_BASE_DIR}/boost_1_46_1/install-64-bit
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
    -d      Create an installer containing install files for package
    -t      Create tag file with exuberant ctags
    -a      Specify if we want to build 32 bit on 64 bit" >&2
}

function ctags()
{
  ${CTAGS_INSTALL_DIR}/bin/ctags -RI --c++-kinds=+p --fields=+iaS --extra=+q --languages=c++ .
  [ -d "${TAGS_DIR}" ] || mkdir -p "${TAGS_DIR}"
  rm ${TAGS_DIR}/${1}
  mv tags ${TAGS_DIR}/${1}
}

# setup the function for controlling innosetup
function innosetup()
{
  #http://www.jrsoftware.org/ishelp/
  #/F"MyProgram-1.0"  - The output filename for the installer
  #/Sbyparam=$p - The sign tool for the installer
  #/Q - The Quiet mode of the compiler
  #/O"My Output" - Override the output directory
  echo "Building the ${VES_SRC_DIR}/dist/win/iss/${ISS_FILENAME} installer."
  if [  $ARCH = "64-bit" ]; then
    /cygdrive/c/Program\ Files\ \(x86\)/Inno\ Setup\ 5/iscc /Q /i${VES_SRC_DIR}/dist/win/iss ${VES_SRC_DIR}/dist/win/iss/${ISS_FILENAME}
  else
    /cygdrive/c/Program\ Files/Inno\ Setup\ 5/iscc /Q /i${VES_SRC_DIR}/dist/win/iss ${VES_SRC_DIR}/dist/win/iss/${ISS_FILENAME}
  fi
}

function source_retrieval()
{
  case ${SOURCE_RETRIEVAL_METHOD} in
    svn)
      cd "${DEV_BASE_DIR}";
      svn co ${SOURCE_URL} "${BASE_DIR}";
      ;;
    hg)
      cd "${DEV_BASE_DIR}";
      hg clone ${SOURCE_URL} "${BASE_DIR}";
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
      # Settings (proxy etc.) for wget can be edited using /etc/wgetrc
      wget ${SOURCE_URL}
      case ${SOURCE_FORMAT} in
        zip)
          unzip `basename ${SOURCE_URL}`;
          rm -f `basename ${SOURCE_URL}`;
          ;;
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
  if [ ! -e "$package" ]; then
    echo "$package is not a package.";
    return;
  fi

  #reset the var controlling wether to install an fpc file
  SKIP_FPC_INSTALL="yes"
  SKIP_PREBUILD="no"
  unset ISS_FILENAME

  #setup the build types unless other wise specified in a build file
  case $PLATFORM in
    Windows)
      BUILD_METHOD=cmake
      BUILD_TARGET=INSTALL
      ;;
    Darwin | Linux )
      BUILD_METHOD=make
      BUILD_TARGET=install
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
      hg)
        if [ -d "${BASE_DIR}" ]; then
          cd "${BASE_DIR}";
          hg update;
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
        ${CONFIGURE} "${CONFIGURE_PARAMS[@]}";
        ;;
      bjam)
        cd "${SOURCE_DIR}";
        "${BJAM_PREBUILD}";
        ;;
      custom)
        cd "${SOURCE_DIR}";
        "${CUSTOM_PREBUILD[@]}";
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
    [ -z "${SOURCE_DIR}" ] && (echo "SOURCE_DIR undefined in package $package"; return)
    [ -d "${BUILD_DIR}" ] || mkdir -p "${BUILD_DIR}"
    [ -z "$multithreading_jobs" ] || JCMD="-j $multithreading_jobs" || MCMD='/p:MultiProcessorCompilation=true /m:"$multithreading_jobs" /p:BuildInParallel=false'
    case ${BUILD_METHOD} in
      devenv)
        cd "${SOURCE_DIR}";
        for sln in "${MSVC_SOLUTION[@]}"; do
          echo "${DEVENV} ${sln} /build $MSVC_CONFIG|$MSVC_PLATFORM"
          "${DEVENV}" "${sln}" /build "$MSVC_CONFIG"'|'"$MSVC_PLATFORM" \
            $( [ -z "${PROJ_STR}" ] && echo "${PROJ_STR}" )
        done
        ;;
      msbuild)
        cd "${BUILD_DIR}";

        if [ -d "${BUILD_TARGET}" ]; then
          MSVC_PROJECT_NAMES+=( "${BUILD_TARGET}" )
        fi

        for name in "${MSVC_PROJECT_NAMES[@]}"; do
          PROJ_STR="$PROJ_STR$name$( [ "$name" != "${MSVC_PROJECT_NAMES[@]: -1}" ] && echo ';' )";
        done

        if [ -z "${PROJ_STR}" ]; then
          "${MSBUILD}" "$MSVC_SOLUTION" "${MCMD}" \
          /p:Configuration="$MSVC_CONFIG" /p:Platform="$MSVC_PLATFORM" /p:TargetFrameworkVersion=v3.5 \
          /p:BuildProjectReferences=false /p:PostBuildEventUseInBuild=true /p:WarningLevel=1 \
          /toolsversion:3.5 /verbosity:Diagnostic
        else
          "${MSBUILD}" "$MSVC_SOLUTION" /t:"$PROJ_STR" "${MCMD}" \
          /p:Configuration="$MSVC_CONFIG" /p:Platform="$MSVC_PLATFORM" /p:TargetFrameworkVersion=v3.5 \
          /p:BuildProjectReferences=false /p:PostBuildEventUseInBuild=true /p:WarningLevel=1 \
          /toolsversion:3.5 /verbosity:Diagnostic
        fi
        ;;
      cmake)
        cd "${BUILD_DIR}";
        echo $BUILD_DIR
        case $PLATFORM in
          Windows)
            if [ -d "${BUILD_TARGET}" ]; then
              MSVC_PROJECT_NAMES+=( "${BUILD_TARGET}" )
            fi

            for name in "${MSVC_PROJECT_NAMES[@]}"; do
              PROJ_STR="$PROJ_STR$name$( [ "$name" != "${MSVC_PROJECT_NAMES[@]: -1}" ] && echo ';' )";
            done
            #http://www.cmake.org/cmake/help/cmake-2-8-docs.html#opt:--builddir
            echo "Build Command: --build ${BUILD_DIR} -- $MSVC_SOLUTION /build $MSVC_CONFIG|$MSVC_PLATFORM /project $PROJ_STR"
            ${CMAKE} --build "${BUILD_DIR}" -- "$MSVC_SOLUTION" /build "$MSVC_CONFIG"'|'"$MSVC_PLATFORM" /project "$PROJ_STR"
            ;;
          Darwin | Linux )
            #http://www.cmake.org/cmake/help/cmake-2-8-docs.html#opt:--builddir
            ${CMAKE} --build "${BUILD_DIR}" -- ${JCMD} ${BUILD_TARGET}
            ;;
        esac
        ;;
      make)
        cd "${BUILD_DIR}";
        ${MAKE} ${JCMD} ${BUILD_TARGET};
        ;;
      scons)
        cd "${BUILD_DIR}";
        ${SCONS} "${SCONS_PARAMS[@]}" ${BUILD_TARGET} ${JCMD};
        ;;
      bjam)
        cd "${SOURCE_DIR}";
        ${BJAM} "${BJAM_PARAMS[@]}" ${BUILD_TARGET} ${JCMD};
        ;;
      *)
        echo "Build method ${BUILD_METHOD} unsupported"
        ;;
    esac
    if [ "${SKIP_FPC_INSTALL}" != "yes" ]; then
      [ -d "${INSTALL_DIR}/lib/flagpoll" ] || mkdir -p "${INSTALL_DIR}/lib/flagpoll"
      case $PLATFORM in
        Windows)
          cp "${VES_SRC_DIR}/dist/win/fpc_deps_files/release/${FPC_FILE}.in" "${INSTALL_DIR}/lib/flagpoll/${FPC_FILE}";
          ;;
        Darwin | Linux)
          cp "${VES_SRC_DIR}/dist/linux/fpc_deps_files/${FPC_FILE}.in" "${INSTALL_DIR}/lib/flagpoll/${FPC_FILE}";
          ;;
      esac
      echo "Installing the fpc file ${FPC_FILE}"
    fi
  fi

  #clean the build
  if [ "${clean_build_dir}" = "yes" ]; then
    [ -z "${BUILD_DIR}" ] && ( echo "BUILD_DIR undefined in package $package"; return; )
    [ -d "${BUILD_DIR}" ] || ( echo "${BUILD_DIR} non existent."; return; )
    case ${BUILD_METHOD} in
      msbuild)
        cd "${BUILD_DIR}";

        for name in "${MSVC_PROJECT_NAMES[@]}"; do
          PROJ_STR="$PROJ_STR${name}:Clean$( [ "$name" != "${MSVC_PROJECT_NAMES[@]: -1}" ] && echo ';' )";
        done

        "${MSBUILD}" "$MSVC_SOLUTION" /t:"$PROJ_STR" "${MCMD}" \
         /p:Configuration="$MSVC_CONFIG" /p:Platform="$MSVC_PLATFORM" \
         /verbosity:Normal /p:WarningLevel=0;
        ;;
      make)
        cd "${BUILD_DIR}";
        ${MAKE} clean;
        ;;
      *)
        echo "Clean method ${BUILD_METHOD} unsupported";
        ;;
    esac
  fi
  
  #Build the ctag files
  if [ "${build_ctag_files}" = "yes" ]; then
    cd ${INSTALL_DIR}/include; 
    ctags $package;
  fi
  
  #Build the installer file
  if [ "${build_installer}" = "yes" ]; then
    [ -z "${BUILD_DIR}" ] && ( echo "BUILD_DIR undefined in package $package"; return; )
    [ -d "${BUILD_DIR}" ] || ( echo "${BUILD_DIR} non existent."; return; )
    [ -z "${INSTALL_DIR}" ] && ( echo "INSTALL_DIR undefined in package $package"; return; )
    [ -d "${INSTALL_DIR}" ] || ( echo "${INSTALL_DIR} non existent."; return; )
    [ -z "${ISS_FILENAME}" ] && ( echo "ISS_FILENAME undefined in package $package"; return; )
    
    case $PLATFORM in
      Windows)
        innosetup;
        ;;
      Darwin)
        ;;
      Linux )
        ;;
    esac
  fi
}

#
# Before we run the script args we set the platform and arch and then set x64 on windows
# if needed
#
platform
arch
windows

while getopts "hkucpbj:U:dta" opts
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
    #export build="yes"  # implied
    ;;
  U)export SVN_USERNAME=$OPTARG;;
  t)export build_ctag_files="yes";;
  d)export build_installer="yes";;
  a)
    platform 32
    arch
    windows
    ;;
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
  *) usage;;
esac
done
shift $(($OPTIND - 1))

[ -d "${DEV_BASE_DIR}" ] || mkdir -p "${DEV_BASE_DIR}"
[ $# -lt 1 ] && bye

echo -e "\n          Kernel: $PLATFORM $ARCH"
if [ $PLATFORM = "Windows" ]; then
  #echo "VCInstallDir: $VCInstallDir"
  echo "DotNETInstallDir: $DotNETInstallDir"
fi
echo -e "    DEV_BASE_DIR: ${DEV_BASE_DIR}"
echo -e "     VES_SRC_DIR: ${VES_SRC_DIR}\n"

#Set the pwd so that for every new build file we can reset to the pwd
#arg=$0; [[ -L $0 ]] && arg=$( stat -f '%Y' "$0" )
#PRESENT_DIR=$( 2>/dev/null cd "${arg%/*}" >&2; echo "`pwd -P`/${arg##*/}" )
#PRESENT_DIR=$( dirname "$PRESENT_DIR" )
PRESENT_DIR=$PWD

#
# Export the install directories for build dependencies and write to file
# Do not put spaces in ${PACKAGE_NAME}
#
export_config_vars()
{
    EXPORT_FILE="${DEV_BASE_DIR}/exports"
    rm -f "${EXPORT_FILE}"
    for f in $*; do
      eval $( sed -n '/^PACKAGE_NAME=/p;/^BASE_DIR=/p;/^INSTALL_DIR=/p;' $f )
      echo "export ${PACKAGE_NAME}_INSTALL_DIR=\"${INSTALL_DIR}\"" >> "${EXPORT_FILE}"
    done

    . "${EXPORT_FILE}"
}

export_config_vars *.build

for p in $@; do
  cd "${PRESENT_DIR}"
  e "${p}";
  cd "${PRESENT_DIR}"
done

exit 0
