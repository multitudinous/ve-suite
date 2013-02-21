#!/bin/bash

function generateEnvironmentFile()
{
    SETENV_COMMAND=""
    SETENV_DELIMITER=""
    prefix=$1
    place_env_file_at_install_root=$2
    env_file_dir=${HOME}

    if [ ${place_env_file_at_install_root} = 1 ]
    then
        env_file_dir=${prefix}
    fi

    env_file_path=${env_file_dir}/VE-SuiteEnv

    if [ $SHELL = "/bin/tcsh" ]
    then
        SETENV_COMMAND="setenv"
        SETENV_DELIMITER=" "
    else
        # assume a Bourne-compatible shell
        SETENV_COMMAND="export"
        SETENV_DELIMITER="="
    fi

    if [ -e ${env_file_path} ]
    then
        rm ${env_file_path}
    fi

    # set up VES_PREFIX
    var_assign_string="${SETENV_COMMAND} VES_PREFIX${SETENV_DELIMITER}"
    var_value_string=${prefix}

    echo ${var_assign_string}${var_value_string} >> ${env_file_path}

    # set up PATH
    var_assign_string="${SETENV_COMMAND} PATH${SETENV_DELIMITER}"
    var_value_string='$VES_PREFIX/bin:$PATH'

    echo ${var_assign_string}${var_value_string} >> ${env_file_path}

    # set up LD_LIBRARY_PATH
    var_assign_string="${SETENV_COMMAND} LD_LIBRARY_PATH${SETENV_DELIMITER}"
    if [ -n "${LD_LIBRARY_PATH}" ]
    then
        var_value_string='$VES_PREFIX/lib:$VES_PREFIX/lib/vtk-5.10:$VES_PREFIX/lib64:$LD_LIBRARY_PATH'
    else
        var_value_string='$VES_PREFIX/lib:$VES_PREFIX/lib/vtk-5.10:$VES_PREFIX/lib64'
    fi

    echo ${var_assign_string}${var_value_string} >> ${env_file_path}

    # set up OSG_FILE_PATH
    var_assign_string="${SETENV_COMMAND} OSG_FILE_PATH${SETENV_DELIMITER}"
    var_value_string='$VES_PREFIX/share/osgBullet/data:$VES_PREFIX/share/osgWorks/data:$VES_PREFIX/share/backdropFX/data'

    echo ${var_assign_string}${var_value_string} >> ${env_file_path}

    # set up OSGNOTIFYLEVEL
    var_assign_string="${SETENV_COMMAND} OSGNOTIFYLEVEL${SETENV_DELIMITER}"
    var_value_string='WARNING'

    echo ${var_assign_string}${var_value_string} >> ${env_file_path}

    # set up PYTHONPATH, if necessary
    current_working_dir=$(pwd)
    cd $1
    wxpython_base_dir=$(find lib64 -type d -name "python*")
    if [ -n "${wxpython_base_dir}" ]
    then
        var_assign_string="${SETENV_COMMAND} PYTHONPATH${SETENV_DELIMITER}"
        if [ -n "${PYTHONPATH}" ]
        then
            var_value_string='/site-packages/wx-2.8-gtk2-unicode:$PYTHONPATH'
        else
            var_value_string='/site-packages/wx-2.8-gtk2-unicode'
        fi
        ves_prefix_string='$VES_PREFIX/'

        echo ${var_assign_string}${ves_prefix_string}${wxpython_base_dir}${var_value_string} >> ${env_file_path}
    fi

    # always set VJ_BASE_DIR
    var_assign_string="${SETENV_COMMAND} VJ_BASE_DIR${SETENV_DELIMITER}"
    var_value_string='$VES_PREFIX'
    echo ${var_assign_string}${var_value_string} >> ${env_file_path}

    cd ${current_working_dir}

    echo ""
    echo "  A file to aid setting up the VE-Suite environment has been written to ${env_file_path}."
    echo "  Source this file in your shell's rc file."
}

generateEnvironmentFile $1 $2

if [ -f "./ves_version" ]
then
    major_version=$(awk 'FNR==1 {print $1;}' ./ves_version)
    minor_version=$(awk 'FNR==2 {print $1;}' ./ves_version)

    if [ ${major_version} == 2 ]
    then
        if [ ${minor_version} == 2 ]
        then
            sed "s|__VE_SUITE_ENV_FILE_PLACEHOLDER__|${env_file_path}|g" ./velauncher.sh.template > "${1}/bin/velauncher.sh"
            chmod +x "${1}/bin/velauncher.sh"
        fi
    fi
fi
