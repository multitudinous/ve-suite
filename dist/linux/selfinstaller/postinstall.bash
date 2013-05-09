#!/bin/bash

function getEnvironmentFileDir()
{
    local install_prefix="${1}"
    local env_file_at_install_root=${2}
    
    if [ ${env_file_at_install_root} = 1 ]
    then
        echo "${install_prefix}"
    else
        echo "${HOME}"
    fi
}

env_file_dir=$(getEnvironmentFileDir ${1} ${2})

sed "s|__VES_INSTALL_PREFIX_PLACEHOLDER__|${1}|g" ./ves-env.sh.in > "${env_file_dir}"/ves-env.sh
sed "s|__VES_INSTALL_PREFIX_PLACEHOLDER__|${1}|g" ./ves-env.csh.in > "${env_file_dir}"/ves-env.csh

printf "A Bourne-compatible environment file has been placed at %s.\n" "${env_file_dir}/ves-env.sh"
printf "A C shell-compatible environment file has been placed at %s.\n" "${env_file_dir}/ves-env.csh"
echo "Source the appropriate file in your shell's rc file."

env_file_path="${env_file_dir}"/ves-env.sh

if [ -f "./ves_version" ]
then
    major_version=$(awk 'FNR==1 {print $1;}' ./ves_version)
    minor_version=$(awk 'FNR==2 {print $1;}' ./ves_version)

    if [[ ${major_version} == 2 && ${minor_version} == 2 ]]
    then
        # this is a 2.2 install
        sed "s|__VE_SUITE_ENV_FILE_PLACEHOLDER__|${env_file_path}|g" ./velauncher.sh.in > "${1}/bin/velauncher.sh"
        chmod +x "${1}/bin/velauncher.sh"
    else
        # this is a 3.x install
        cp ./README.txt "${1}"
        sed "s|__VE_SUITE_ENV_FILE_PLACEHOLDER__|${env_file_path}|g" ./ves-cluster-control.sh.in > "${1}/bin/ves-cluster-control.sh"
        chmod +x "${1}/bin/ves-cluster-control.sh"
        sed "s|__VE_SUITE_ENV_FILE_PLACEHOLDER__|${env_file_path}|g" ./launch-ves_xplorer-master.sh.in > "${1}/bin/launch-ves_xplorer-master.sh"
        chmod +x "${1}/bin/launch-ves_xplorer-master.sh"
        sed "s|__VE_SUITE_ENV_FILE_PLACEHOLDER__|${env_file_path}|g" ./launch-ves_xplorer-rendernode.sh.in > "${1}/bin/launch-ves_xplorer-rendernode.sh"
        chmod +x "${1}/bin/launch-ves_xplorer-rendernode.sh"
        sed "s|__VE_SUITE_ENV_FILE_PLACEHOLDER__|${env_file_path}|g" ./launch-ves_xplorer-desktop.sh.in > "${1}/bin/launch-ves_xplorer-desktop.sh"
        chmod +x "${1}/bin/launch-ves_xplorer-desktop.sh"
        sed "s|__VES_INSTALL_PREFIX_PLACEHOLDER__|${1}|g" ./ves-aliases.sh.example.in > "${1}/ves-aliases.sh.example"
        sed "s|__VES_INSTALL_PREFIX_PLACEHOLDER__|${1}|g" ./ves-aliases.csh.example.in > "${1}/ves-aliases.csh.example"
    fi
fi
