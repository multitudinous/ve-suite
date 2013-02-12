#!/bin/bash

VES_INSTALL_PREFIX=""

function usage()
{
    echo "usage: $0 -p <prefix> [ -h ]"
}

place_env_file_at_install_root=1

while getopts "p:h" SCRIPT_ARGS
do
    case ${SCRIPT_ARGS} in
    p)
        VES_INSTALL_PREFIX=${OPTARG}
        ;;
    h)
        place_env_file_at_install_root=0
        ;;
    ?)
        usage
        exit 1
    esac
done

if [ -z ${VES_INSTALL_PREFIX} ]
then
    usage
    exit 1
fi

if [ -d "${VES_INSTALL_PREFIX}" ]
then
    if [ "$(ls -A ${VES_INSTALL_PREFIX})" ]
    then
        echo "ERROR! ${VES_INSTALL_PREFIX} is not empty. Bailing out..."
        exit 1
    fi
else
    echo "${VES_INSTALL_PREFIX} does not exist. Creating it..."
    mkdir -p ${VES_INSTALL_PREFIX}
fi

# check for at least 2.5 GiB of free space in VES_INSTALL_PREFIX
if [ $(df -P ${VES_INSTALL_PREFIX} | awk 'FNR == 2 {print $4}') -lt 2621440 ]
then
    echo "ERROR!"
    echo "  VE-Suite needs at least 2.5 GiB in ${VES_INSTALL_PREFIX}"
    echo "  to install. Free some space on the volume containing"
    echo "  ${VES_INSTALL_PREFIX} and try again."
    exit 1
fi 

# Install VE-Suite
echo "Installing..."
echo "...Stage 1..."
tar xf ./ve-suite.tar -C ${VES_INSTALL_PREFIX}

echo "...Stage 2..."
tar xf ./ve-suite-deps.tar -C ${VES_INSTALL_PREFIX}

if [ "$(ls -A extra/)" ]
then
    echo "...Stage 3..."
    cd extra
    for extra_tar in *.tar
    do
        tar xf ./${extra_tar} -C ${VES_INSTALL_PREFIX}
    done
    cd ..
fi

echo "Finished!"
./postinstall.bash ${VES_INSTALL_PREFIX} ${place_env_file_at_install_root}

chmod -R 755 ${VES_INSTALL_PREFIX}
