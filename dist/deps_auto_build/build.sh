#!/bin/bash

if [ -z "${VES_SRC_DIR}" ]; then
  export VES_SRC_DIR=$PWD/../../
fi

VES_22_PACKAGES+=("osg")
VES_22_PACKAGES+=("VTK")
VES_22_PACKAGES+=("boost.1.44")
VES_22_PACKAGES+=("juggler.3.0")
VES_22_PACKAGES+=("osgbullet")
VES_22_PACKAGES+=("bullet")
VES_22_PACKAGES+=("ace+tao")
VES_22_PACKAGES+=("cppdom")
VES_22_PACKAGES+=("gmtl")
VES_22_PACKAGES+=("osgworks")
VES_22_PACKAGES+=("poco")
VES_22_PACKAGES+=("xerces")

VES_30_PACKAGES+=("osg")
VES_30_PACKAGES+=("vtk")
VES_30_PACKAGES+=("boost")
VES_30_PACKAGES+=("juggler")
VES_30_PACKAGES+=("osgbullet")
VES_30_PACKAGES+=("bullet")
VES_30_PACKAGES+=("ace+tao")
VES_30_PACKAGES+=("cppdom")
VES_30_PACKAGES+=("sdl")
VES_30_PACKAGES+=("osgworks")
VES_30_PACKAGES+=("poco")
VES_30_PACKAGES+=("switchwire")
VES_30_PACKAGES+=("storyteller")
VES_30_PACKAGES+=("crunchstore")
VES_30_PACKAGES+=("propertystore")
VES_30_PACKAGES+=("osgephemeris")
VES_30_PACKAGES+=("bdfx")
VES_30_PACKAGES+=("xerces")

#
# Define the platform
#
function platform()
{
  OS_ARCH=x64
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
        OS_ARCH=x86
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
        CMAKE_GENERATOR="${CMAKE_GENERATOR} Win64"
      fi
      ;;
    *)
      echo "Unrecognized Architecture: $ARCH" >&2
      kill -SIGINT $$
      ;;
  esac
  export ARCH
}

#
# define the command used for downloading sources
#
function wget()
{
  case $PLATFORM in
    Windows)
      WGET_METHOD=$( which wget )
      ;;
    Darwin)
      WGET_METHOD=$( which curl )
      WGET_METHOD=${WGET_METHOD}" -O --fail -L"
      ;;
    Linux )
      WGET_METHOD=$( which wget )
      #WGET_METHOD=curl\ -O
      ;;
  esac
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

    #
    #Setup OSG 3rd party directory
    #
    #if [ ${ARCH} = "32-bit" ]; then
    #  export THIRD_PARTY="C:\dev\deps\3rdParty_x86_x64\x86"
    #else
    #  export THIRD_PARTY="C:\dev\deps\3rdParty_x86_x64\x64"
    #fi
  fi
}

#
# DEV_BASE_DIR defines the base directory for all development packages.
# May be overriden with a shell variable
#
[ -z "${DEV_BASE_DIR}" ] && export DEV_BASE_DIR="${HOME}/dev/deps"

#
# Some useful global variables
#
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

#
# help statement for script usage
#
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
                On linux this copies everything into DEPS_INSTALL_DIR
                On windows this should run the respective iss file
    -t      Create tag file with exuberant ctags
    -a      Specify if we want to build 32 bit on 64 bit
    -F      Install all of the valid deps that have been built " >&2
}

#
# execute ctags
#
function ctags()
{
  ${CTAGS_INSTALL_DIR}/bin/ctags -RI --c++-kinds=+p --fields=+iaS --extra=+q --languages=c++ .
  [ -d "${TAGS_DIR}" ] || mkdir -p "${TAGS_DIR}"
  rm ${TAGS_DIR}/${1}
  mv tags ${TAGS_DIR}/${1}
}

#
# unset all of the vars for building
#
function unsetvars()
{
  SKIP_FPC_INSTALL="yes"
  SKIP_PREBUILD="no"
  unset ISS_FILENAME
  unset CMAKE_PARAMS
  if [ ! -z "${CMAKE_GENERATOR}" ]; then CMAKE_PARAMS+=( -G "${CMAKE_GENERATOR}" ); fi
  unset CONFIGURE_PARAMS
  unset MSVC_PROJECT_NAMES
  unset MSVC_SOLUTION
  unset SCONS_PARAMS
  unset BJAM_PARAMS
  unset INNO_PARAMS
  unset POST_RETRIEVAL_METHOD
  unset POST_BUILD_METHOD
  unset SOURCE_REVISION
  unset GIT_BRANCH_VERSION
  unset PREBUILD_METHOD
  unset FPC_FILE
  unset VES_INSTALL_PARAMS
}

#
# search all build files
#
function findvaliddeps()
{
    echo "Preparing to install valid dependencies."
    validatedeps *.build
    echo "Done installing valid dependencies."
}

#
# install all of the valid deps
#
function validatedeps()
{
    if [ ! -d "${DEPS_INSTALL_DIR}" ]; then mkdir -p "${DEPS_INSTALL_DIR}"; fi

    for f in $*; do
      . "$f"
      if [ -d "${INSTALL_DIR}" ]; then
        case $PLATFORM in
          Windows )
            if [ -z "${ISS_FILENAME}" ]; then echo "ISS_FILENAME undefined in package $package"; return; fi
            innosetup;
            ;;
          Darwin | Linux )
            echo "Installing ${INSTALL_DIR}/. to ${DEPS_INSTALL_DIR}"
            cp -R "${INSTALL_DIR}"/. "${DEPS_INSTALL_DIR}"
            ;;
        esac
      fi
    done
}

#
# setup the function for controlling innosetup
#
function innosetup()
{
  #http://www.jrsoftware.org/ishelp/
  #/F"MyProgram-1.0"  - The output filename for the installer
  #/Sbyparam=$p - The sign tool for the installer
  #/Q - The Quiet mode of the compiler
  #/O"My Output" - Override the output directory
  echo "Building the ${VES_SRC_DIR}/dist/win/iss/${ISS_FILENAME} installer."
  INNO_PARAMS+=( "/dvesAutoBuild=1" )
  INNO_PARAMS+=( "/dINSTALLERINSTALLLOCATION=${DEV_BASE_DIR}" )
  if [  $ARCH = "64-bit" ]; then
    INNO_PARAMS+=( "/dMSVCVERSION=msvc-9.0-sp1-x64" )
    INNO_PARAMS+=( "/dLIBDIR=lib64" )
    INNO_PARAMS+=( "/dBUILDDIR=x64" )
    INNO_PARAMS+=( "/dDISTDIR=Win64" )
  else
    INNO_PARAMS+=( "/dMSVCVERSION=msvc-9.0-sp1-x86" )
    INNO_PARAMS+=( "/dLIBDIR=lib" )
    INNO_PARAMS+=( "/dBUILDDIR=x86" )
    INNO_PARAMS+=( "/dDISTDIR=Win32" )
  fi
  INNO_PARAMS+=( "/dVESGROUPNAME=VE-Suite" )
  INNO_PARAMS+=( "/dVEDEVHOME=${VES_SRC_DIR}" )

  if [  $OS_ARCH = "x64" ]; then
    /cygdrive/c/Program\ Files\ \(x86\)/Inno\ Setup\ 5/iscc /Q  "${INNO_PARAMS[@]}" /i${VES_SRC_DIR}/dist/win/iss ${VES_SRC_DIR}/dist/win/iss/${ISS_FILENAME}
  else
    /cygdrive/c/Program\ Files/Inno\ Setup\ 5/iscc /Q /i${VES_SRC_DIR}/dist/win/iss ${VES_SRC_DIR}/dist/win/iss/${ISS_FILENAME}
  fi
}

function source_retrieval()
{
  case ${SOURCE_RETRIEVAL_METHOD} in
    svn)
      cd "${DEV_BASE_DIR}";
      SVN_CO="svn co"

      if [ -n "${SOURCE_REVISION:+x}" ]; then
        SVN_CO="${SVN_CO} -r ${SOURCE_REVISION}";
        echo "using custom command ${SVN_CO}" 
      fi

      if [ $PLATFORM = "Windows" ]; then
        TEMP_BASE_DIR=`cygpath -u "${BASE_DIR}"`
        ${SVN_CO} ${SOURCE_URL} "${TEMP_BASE_DIR}"
      else
        ${SVN_CO} ${SOURCE_URL} "${BASE_DIR}"
      fi
      ;;
    hg)
      cd "${DEV_BASE_DIR}";
      hg clone ${SOURCE_URL} "${BASE_DIR}";
      ;;
    git)
      cd "${DEV_BASE_DIR}";
      git clone ${SOURCE_URL} "${BASE_DIR}";
      if [ -n "${GIT_BRANCH_VERSION:+x}" ]; then
        cd "${BASE_DIR}";
        git checkout -t origin/${GIT_BRANCH_VERSION}
        git pull
        echo "using custom command ${GIT_BRANCH_VERSION}" 
      fi 
      ;;
    private-svn)
      cd "${DEV_BASE_DIR}";
      SVN_CO="svn co"
      if [ -n "${SOURCE_REVISION:+x}" ]; then
        SVN_CO="${SVN_CO} -r ${SOURCE_REVISION}";
      fi
      ${SVN_CO} ${SOURCE_URL} "${BASE_DIR}" --username="${SVN_USERNAME}";
      ;;
    wget)
      [ -z "${SOURCE_FORMAT}" ] && ( echo "SOURCE_FORMAT undefined in package $package"; return; )
      cd "${DEV_BASE_DIR}";
      if [ -d "${BASE_DIR}" ]; then
        echo "We have already downloaded $package for ${BASE_DIR}";
        return;
      fi
      # Settings (proxy etc.) for wget can be edited using /etc/wgetrc 
      echo "${WGET_METHOD}"
      eval "${WGET_METHOD}" "${SOURCE_URL}"
      case ${SOURCE_FORMAT} in
        zip)
          unzip `basename ${SOURCE_URL}`;
          rm -f `basename ${SOURCE_URL}`;
          #if [ -d "${BASE_DIR}" ]; then
          #  echo "The BASE_DIR for $package already exists.";
          #else
          #  mkdir -p "${BASE_DIR}";
          #fi
          ;;
        tgz)
          TEMPBASENAME=`basename ${SOURCE_URL}`
          PACKAGE_BASE_DIR_NAME=$( tar tf "${TEMPBASENAME}" | grep -o '^[^/]\+' | sort -u )
          tar xvfz "${TEMPBASENAME}";
          rm -f `basename ${SOURCE_URL}`;
          if [ -d "${BASE_DIR}" ]; then
            echo "The BASE_DIR for $package already exists.";
          else
            mv "${PACKAGE_BASE_DIR_NAME}" "${BASE_DIR}";
          fi
          ;;
        bz2)
          TEMPBASENAME=`basename ${SOURCE_URL}`
          PACKAGE_BASE_DIR_NAME=$( tar tf "${TEMPBASENAME}" | grep -o '^[^/]\+' | sort -u )
          tar xvfjk "${TEMPBASENAME}";
          rm -f "${TEMPBASENAME}";
          if [ -d "${BASE_DIR}" ]; then
            echo "The BASE_DIR for $package already exists.";
          else
            mv "${PACKAGE_BASE_DIR_NAME}" "${BASE_DIR}";
          fi
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

  #
  #reset the vars for the builds
  #
  unsetvars;

  #
  # setup the build types unless other wise specified in a build file
  #
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

  #
  # check to make sure that the base dir is defined for the package
  #
  if [ -z "${BASE_DIR}" ]; then echo "BASE_DIR undefined in package $package"; return; fi

  #
  # checkout the source for download the source
  #
  if [ "${check_out_source}" = "yes" ]; then
    if [ -z "${SOURCE_URL}" ]; then echo "SOURCE_URL undefined in package $package"; return; fi
    if [ -z "${SOURCE_RETRIEVAL_METHOD}" ]; then echo "SOURCE_RETRIEVAL_METHOD undefined in package $package"; return; fi
    source_retrieval;
    if [ ! -z "${POST_RETRIEVAL_METHOD}" ]; then
        echo "Running the POST_RETRIEVAL_METHOD for $package."
        cd "${SOURCE_DIR}";
        eval "${POST_RETRIEVAL_METHOD}"
    fi
  fi

  #
  # update the source if needed
  #
  if [ "${update_source}" = "yes" ]; then
    if [ -z "${SOURCE_RETRIEVAL_METHOD}" ]; then echo "SOURCE_RETRIEVAL_METHOD undefined in package $package"; return; fi

    case ${SOURCE_RETRIEVAL_METHOD} in
      svn | private-svn)
        if [ -d "${BASE_DIR}" ]; then
            # If we have defined svn version there is no need to update the code
            if [ ! -n "${SOURCE_REVISION:+x}" ]; then
                cd "${BASE_DIR}";
                svn up;
            fi

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
          hg pull;
          hg update;
        else
          echo "${BASE_DIR} non-existent, checking out ...."
          [ -z "${SOURCE_URL}" ] && ( echo "SOURCE_URL undefined in package $package"; return; )
          [ -z "${SOURCE_RETRIEVAL_METHOD}" ] && ( echo "SOURCE_RETRIEVAL_METHOD undefined in package $package"; return; )

          source_retrieval;
        fi
        ;;
      git)
        if [ -d "${BASE_DIR}" ]; then
          cd "${BASE_DIR}";
          git pull;
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

  #
  # prebuild for the package
  #
  if [ "${prebuild}" = "yes" ] && [ "${SKIP_PREBUILD}" != "yes" ]; then
    if [ -z "${BUILD_DIR}" ]; then echo "BUILD_DIR undefined in package $package"; return; fi
    if [ -z "${PREBUILD_METHOD}" ]; then echo "PREBUILD_METHOD undefined in package $package"; return; fi
    if [ -z "${SOURCE_DIR}" ]; then echo "SOURCE_DIR undefined in package $package"; return; fi
    if [ ! -d "${SOURCE_DIR}" ]; then echo "SOURCE_DIR does not exist for package $package"; return; fi
    if [ ! -d "${BUILD_DIR}" ]; then mkdir -p "${BUILD_DIR}"; fi

    case ${PREBUILD_METHOD} in
      cmake)
        cd "${BUILD_DIR}";
        echo $BUILD_DIR
        which cmake
        ${CMAKE} "${SOURCE_DIR}" "${CMAKE_PARAMS[@]}";
        echo ${CMAKE_PARAMS[@]}
        echo "done prebuild"
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
    if [ -z "${BUILD_DIR}" ]; then echo "BUILD_DIR undefined in package $package"; return; fi
    if [ -z "${BUILD_METHOD}" ]; then echo "BUILD_METHOD undefined in package $package"; return; fi
    if [ -z "${SOURCE_DIR}" ]; then echo "SOURCE_DIR undefined in package $package"; return; fi
    if [ ! -d "${SOURCE_DIR}" ]; then echo "SOURCE_DIR does not exist for package $package"; return; fi
    if [ ! -d "${BUILD_DIR}" ]; then mkdir -p "${BUILD_DIR}"; fi
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
            if [ ! -z "${BUILD_TARGET}" ]; then
              MSVC_PROJECT_NAMES+=( "${BUILD_TARGET}" )
            fi

            unset PROJ_STR
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
        echo "Build Comand: ${BJAM_PARAMS[@]} ${BUILD_TARGET} ${JCMD}"
        ${BJAM} "${BJAM_PARAMS[@]}" ${BUILD_TARGET} ${JCMD};
        ;;
      *)
        echo "Build method ${BUILD_METHOD} unsupported"
        ;;
    esac
    if [ "${SKIP_FPC_INSTALL}" != "yes" ]; then
      if [ ! -d "${INSTALL_DIR}/lib/flagpoll" ]; then mkdir -p "${INSTALL_DIR}/lib/flagpoll"; fi
      case $PLATFORM in
        Windows)
          cp "${VES_SRC_DIR}/dist/win/fpc_deps_files/release/${FPC_FILE}.in" "${INSTALL_DIR}/lib/flagpoll/${FPC_FILE}";
          echo "Installing ${VES_SRC_DIR}/dist/win/fpc_deps_files/release/${FPC_FILE}.in to ${INSTALL_DIR}/lib/flagpoll/${FPC_FILE}"
          ;;
        Darwin | Linux)
          cp "${VES_SRC_DIR}/dist/linux/fpc_deps_files/${FPC_FILE}.in" "${INSTALL_DIR}/lib/flagpoll/${FPC_FILE}";
          echo "Installing ${VES_SRC_DIR}/dist/linux/fpc_deps_files/${FPC_FILE}.in to ${INSTALL_DIR}/lib/flagpoll/${FPC_FILE}"
          ;;
      esac
      echo "Installing the fpc file ${FPC_FILE}"
    fi
    if [ ! -z "${POST_BUILD_METHOD}" ]; then
      echo "Running the POST_BUILD_METHOD for $package."
      cd "${BUILD_DIR}";
      eval "${POST_BUILD_METHOD}"
    fi
  fi

  #
  # clean the build
  #
  if [ "${clean_build_dir}" = "yes" ]; then
    if [ -z "${BUILD_DIR}" ]; then echo "BUILD_DIR undefined in package $package"; return; fi
    if [ ! -d "${BUILD_DIR}" ]; then echo "${BUILD_DIR} non existent."; return; fi

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
      cmake)
        cd "${BUILD_DIR}";
        echo $BUILD_DIR
        case $PLATFORM in
          Windows)
            echo "I am not sure how to clean a cmake build on windows"
            #${CMAKE} --build "${BUILD_DIR}" -- "$MSVC_SOLUTION" /build "$MSVC_CONFIG"'|'"$MSVC_PLATFORM" /project "$PROJ_STR"
            ;;
          Darwin | Linux )
            #http://www.cmake.org/cmake/help/cmake-2-8-docs.html#opt:--builddir
            ${CMAKE} --build "${BUILD_DIR}" -- clean
            ;;
        esac
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
  
  #
  # Build the installer file
  #
  if [ "${build_installer}" = "yes" ]; then
    #if [ -z "${BUILD_DIR}" ]; then echo "BUILD_DIR undefined in package $package"; return; fi
    #if [ ! -d "${BUILD_DIR}" ]; then echo "${BUILD_DIR} non existent."; return; fi
    if [ -z "${INSTALL_DIR}" ]; then echo "INSTALL_DIR undefined in package $package"; return; fi
    if [ ! -d "${INSTALL_DIR}" ]; then echo "${INSTALL_DIR} non existent."; return; fi
    if [ ! -d "${DEPS_INSTALL_DIR}" ]; then mkdir -p "${DEPS_INSTALL_DIR}"; fi
    
    case $PLATFORM in
      Windows )
        if [ -z "${ISS_FILENAME}" ]; then echo "ISS_FILENAME undefined in package $package"; return; fi
        innosetup;
        ;;
      Darwin | Linux )
        cd "${INSTALL_DIR}"
        cp -R "${INSTALL_DIR}"/. "${DEPS_INSTALL_DIR}"
        cd "${PRESENT_DIR}"
        ;;
    esac
  fi

  #
  # Build the installer file
  #
  if [ "${build_auto_installer}" = "yes" ]; then
    #if [ -z "${BUILD_DIR}" ]; then echo "BUILD_DIR undefined in package $package"; return; fi
    #if [ ! -d "${BUILD_DIR}" ]; then echo "${BUILD_DIR} non existent."; return; fi
    if [ -z "${INSTALL_DIR}" ]; then echo "INSTALL_DIR undefined in package $package"; return; fi
    if [ ! -d "${INSTALL_DIR}" ]; then echo "${INSTALL_DIR} non existent."; return; fi
    if [ ! -d "${DEPS_INSTALL_DIR}" ]; then mkdir -p "${DEPS_INSTALL_DIR}"; fi
    
    case $PLATFORM in
      Windows )
        if [ -z "${ISS_FILENAME}" ]; then echo "ISS_FILENAME undefined in package $package"; return; fi
        innosetup;
        ;;
      Darwin | Linux )
          for p in "${VES_30_PACKAGES[@]}"; do
              unsetvars
              #echo "${DEVENV} ${sln} /build $MSVC_CONFIG|$MSVC_PLATFORM"
              #"${DEVENV}" "${sln}" /build "$MSVC_CONFIG"'|'"$MSVC_PLATFORM" \
              #  $( [ -z "${PROJ_STR}" ] && echo "${PROJ_STR}" )
              cd "${PRESENT_DIR}"
              . "${p}".build;
              if [ ! -z "${VES_INSTALL_PARAMS}" ]; then
                  echo "Installing ${INSTALL_DIR}/. to ${DEPS_INSTALL_DIR}"
                  cd "${INSTALL_DIR}"
                  rsync -a -R "${VES_INSTALL_PARAMS[@]}" "${DEPS_INSTALL_DIR}"/.
              fi
              cd "${PRESENT_DIR}"
          done
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
wget

#
# setup deps install dir
#
[ -z "${DEPS_INSTALL_DIR}" ] && export DEPS_INSTALL_DIR="${HOME}/dev/deps/install-${PLATFORM}"

#
# execute the script
#
while getopts "hkucpbj:U:tdgaF" opts
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
  g)export build_auto_installer="yes";;
  a)
    platform 32
    arch
    windows
    ;;
  F)# install all of the valid deps
    findvaliddeps
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
#[ $# -lt 1 ] && [ "${build_installer}" = "no" ] && bye
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
      #eval $( sed -n '/^PACKAGE_NAME=/p;/^BASE_DIR=/p;/^INSTALL_DIR=/p;' $f )
      unsetvars;
      . "${f}"
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
