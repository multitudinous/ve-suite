#!/bin/bash

function usage()
{
echo "
  Usage: $0 -p <prefix> [ -h ]

  OPTIONS:
   -p <prefix> : the full path to the VE-Suite install directory (REQUIRED)
   -h          : if this flag is specified, place the environment file
                 in your home directory instead of at the root of the
                 VE-Suite install (OPTIONAL)
"
}

INSTALL_PREFIX=""
place_env_file_at_install_root=1

while getopts "p:h" SCRIPT_ARGS
do
    case ${SCRIPT_ARGS} in
    p)
        INSTALL_PREFIX=${OPTARG}
        ;;
    h)
        place_env_file_at_install_root=0
        ;;
    ?)
        usage
        exit 1
        ;;
    esac
done

if [ -z ${INSTALL_PREFIX} ]
then
    usage
    exit 1
fi

# check for 2.5 GiB of free space in /tmp
if [ $(df -P /tmp | awk 'FNR == 2 {print $4}') -lt 2621440 ]
then
    echo "ERROR!"
    echo "  The VE-Suite installer needs at least 2.5 GiB"
    echo "  of free space in /tmp to decompress its payload."
    echo "  Free some space on the volume containing /tmp"
    echo "  and try again."
    exit 1
fi

echo "Decompressing..."

DECOMPRESS_DIR=$(mktemp -d /tmp/ves-installer-decompress-dir.XXXXXX)
ARCHIVE=$(awk '/^__ARCHIVE_BELOW_THIS_LINE__/ { print NR + 1; exit 0; }' $0)

tail -n+${ARCHIVE} $0 | bzcat | tar x -C ${DECOMPRESS_DIR}

echo "Finished decompressing"

CURRENT_WORKING_DIR=$(pwd)
cd ${DECOMPRESS_DIR}
if [ ${place_env_file_at_install_root} = 1 ]
then
    ./install.bash -p ${INSTALL_PREFIX}
else
    ./install.bash -p ${INSTALL_PREFIX} -h
fi

cd ${CURRENT_WORKING_DIR}
rm -rf ${DECOMPRESS_DIR}

exit 0

__ARCHIVE_BELOW_THIS_LINE__
