#!/bin/bash

function usage()
{
echo "
  Usage: $0 -p <prefix> [ -e ] [ -h ]

  OPTIONS:
   -p <prefix> : the full path to the VE-Suite install directory (REQUIRED)
   -e          : if this flag is specified, place the environment file
                 in your home directory instead of at the root of the
                 VE-Suite install (OPTIONAL)
   -h          : show this help info
"
}

INSTALL_PREFIX=""
place_env_file_at_install_root=1

while getopts "p:eh" SCRIPT_ARGS
do
    case ${SCRIPT_ARGS} in
    p)
        INSTALL_PREFIX=${OPTARG}
        ;;
    e)
        place_env_file_at_install_root=0
        ;;
    h)
        usage
        exit 0
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

if [ ! -d ~/.tmp ]
then
    mkdir ~/.tmp
fi

# check for 2.5 GiB of free space in ~/.tmp
if [ $(df -P ~/.tmp | awk 'FNR == 2 {print $4}') -lt 2621440 ]
then
    echo "ERROR!"
    echo "  The VE-Suite installer needs at least 2.5 GiB"
    echo "  of free space in ${HOME}/.tmp to decompress its payload."
    echo "  Free some space on the volume containing ${HOME}/.tmp"
    echo "  and try again."
    exit 1
fi

echo "Decompressing..."

DECOMPRESS_DIR=$(mktemp -d "${HOME}/.tmp/ves-installer-decompress-dir.XXXXXX")
ARCHIVE=$(awk '/^__ARCHIVE_BELOW_THIS_LINE__/ { print NR + 1; exit 0; }' $0)

tail -n+${ARCHIVE} $0 | bzcat | tar x -C ${DECOMPRESS_DIR}

echo "Finished decompressing"

pushd $(pwd) > /dev/null
cd ${DECOMPRESS_DIR}
if [ ${place_env_file_at_install_root} = 1 ]
then
    ./install.bash -p ${INSTALL_PREFIX}
else
    ./install.bash -p ${INSTALL_PREFIX} -e
fi
popd > /dev/null

rm -rf ${DECOMPRESS_DIR}

exit 0

__ARCHIVE_BELOW_THIS_LINE__
