#!/bin/bash

function usage()
{
echo "
  Usage: $0 -p <VE-Suite install prefix> -d <deps tar file> [ -x <extra tar file> ] [ -s <extra string> ]

    Options:

    -p  The full path to the VE-Suite install directory (REQUIRED)
    -d  The full path to a tar file of VE-Suite's dependencies (REQUIRED)
    -t  The full path to the VE-Suite source tree (REQUIRED)
    -x  Path to a tar file to add to the installer's payload (OPTIONAL)
        NOTE: The -x flag may be used as many times as is necessary
    -s  An extra string to include in the installer file name (OPTIONAL)
"
}

SCRIPT_DIR=$(dirname $0)

VES_INSTALL_PREFIX=""
VES_DEPS_TAR_FILE=""
VES_SOURCE_TREE_DIR=""
EXTRA_PAYLOAD_TAR_FILES=()
extra_filename_string=""

while getopts "p:d:t:x:s:" SCRIPT_ARGS
do
    case ${SCRIPT_ARGS} in
    p)
        VES_INSTALL_PREFIX=${OPTARG}
        ;;
    d)
        VES_DEPS_TAR_FILE=${OPTARG}
        ;;
    t)
        VES_SOURCE_TREE_DIR=${OPTARG}
        ;;
    x)
        EXTRA_PAYLOAD_TAR_FILES=( ${EXTRA_PAYLOAD_TAR_FILES[@]} ${OPTARG} )
        ;;
    s)
        extra_filename_string=${OPTARG}
        ;;
    ?)
        usage
        exit 1
        ;;
    esac
done

if [ -z "${VES_INSTALL_PREFIX}" ]
then
    usage
    exit 1
fi

if [ -z "${VES_DEPS_TAR_FILE}" ]
then
    usage
    exit 1
fi

if [ -z "${VES_SOURCE_TREE_DIR}" ]
then
    usage
    exit 1
fi

INSTALLER_ROOT_DIR=$(mktemp -d /tmp/ves-installer-rootdir.XXXXXX)
INSTALLER_PAYLOAD_DIR="${INSTALLER_ROOT_DIR}/installer/payload"
INSTALLER_EXTRA_PAYLOAD_DIR="${INSTALLER_PAYLOAD_DIR}/extra"

if [ ! -e "${VES_SOURCE_TREE_DIR}/src/ves/VEConfig.h" ]
then
    echo "ERROR! Can't find VEConfig.h, bailing out!"
    rm -rf ${INSTALLER_ROOT_DIR}
    exit 1
fi

ves_major_version=$(awk '/VES_MAJOR_VERSION/ {print $3;}' ${VES_SOURCE_TREE_DIR}/src/ves/VEConfig.h)
ves_minor_version=$(awk '/VES_MINOR_VERSION/ {print $3;}' ${VES_SOURCE_TREE_DIR}/src/ves/VEConfig.h)
ves_patch_version=$(awk '/VES_PATCH_VERSION/ {print $3;}' ${VES_SOURCE_TREE_DIR}/src/ves/VEConfig.h)

git_revision=$(git --git-dir ${VES_SOURCE_TREE_DIR}/.git rev-parse HEAD | cut -c1-10)

if [ ! -z "${extra_filename_string}" ]
then
    # if the string is not empty, prepend a hyphen
    extra_filename_string="-${extra_filename_string}"
fi

installer_file_name="VE-SuiteInstall-${ves_major_version}.${ves_minor_version}.${ves_patch_version}-${git_revision}${extra_filename_string}.bash"

echo "Creating temporary directory structure at ${INSTALLER_ROOT_DIR}..."
mkdir -p ${INSTALLER_EXTRA_PAYLOAD_DIR}

CURRENT_WORKING_DIR=$(pwd)

echo "Adding version information..."
echo ${ves_major_version} >> "${INSTALLER_PAYLOAD_DIR}/ves_version"
echo ${ves_minor_version} >> "${INSTALLER_PAYLOAD_DIR}/ves_version"
echo ${ves_patch_version} >> "${INSTALLER_PAYLOAD_DIR}/ves_version"

echo "Adding install script..."
cp ${SCRIPT_DIR}/install.bash ${INSTALLER_PAYLOAD_DIR}

echo "Adding post-install script..."
cp ${SCRIPT_DIR}/postinstall.bash ${INSTALLER_PAYLOAD_DIR}

echo "Adding script templates..."
cp ${SCRIPT_DIR}/velauncher.sh.in ${INSTALLER_PAYLOAD_DIR}
cp ${SCRIPT_DIR}/ves-cluster-control.sh.in ${INSTALLER_PAYLOAD_DIR}
cp ${SCRIPT_DIR}/launch-ves_xplorer-master.sh.in ${INSTALLER_PAYLOAD_DIR}
cp ${SCRIPT_DIR}/launch-ves_xplorer-rendernode.sh.in ${INSTALLER_PAYLOAD_DIR}
cp ${SCRIPT_DIR}/launch-ves_xplorer-desktop.sh.in ${INSTALLER_PAYLOAD_DIR}

echo "Adding environment templates..."
cp ${SCRIPT_DIR}/ves-env.sh.in ${INSTALLER_PAYLOAD_DIR}
cp ${SCRIPT_DIR}/ves-env.csh.in ${INSTALLER_PAYLOAD_DIR}
cp ${SCRIPT_DIR}/ves-aliases.sh.example.in ${INSTALLER_PAYLOAD_DIR}
cp ${SCRIPT_DIR}/ves-aliases.csh.example.in ${INSTALLER_PAYLOAD_DIR}

cp ${SCRIPT_DIR}/README.txt ${INSTALLER_PAYLOAD_DIR}

# tar up the VE-Suite install and add it to the payload directory
echo "Archiving the VE-Suite install..."
cd ${VES_INSTALL_PREFIX}
tar cf ${INSTALLER_PAYLOAD_DIR}/ve-suite.tar bin/ lib64/ share/

# add the dependencies
echo "Adding dependencies..."
cp ${VES_DEPS_TAR_FILE} ${INSTALLER_PAYLOAD_DIR}/ve-suite-deps.tar

# any any extra tar files to the payload
for tarfile in ${EXTRA_PAYLOAD_TAR_FILES[@]}
do
    echo "Adding extra archive ${tarfile} to payload..."
    cp ${tarfile} ${INSTALLER_EXTRA_PAYLOAD_DIR}
done

echo "Archiving the payload..."
cd ${INSTALLER_PAYLOAD_DIR}
tar cf ../payload.tar ./*
cd ..

if [ -e "payload.tar" ]
then
    echo "Compressing the payload..."
    bzip2 payload.tar

    if [ -e "payload.tar.bz2" ]
    then
        echo "Creating the self-extracting installer..."
        cat ${CURRENT_WORKING_DIR}/${SCRIPT_DIR}/decompress.bash payload.tar.bz2 > ${CURRENT_WORKING_DIR}/${installer_file_name}
        chmod +x ${CURRENT_WORKING_DIR}/${installer_file_name}
    else
        echo "payload.tar.bz2 does not exist!"
        exit 1
    fi
else
    echo "payload.tar does not exist!"
    exit 1
fi

echo "VE-Suite self-extracting installer created at ${CURRENT_WORKING_DIR}/${installer_file_name}"

echo "Cleaning up..."
rm -rf ${INSTALLER_ROOT_DIR}

exit 0
