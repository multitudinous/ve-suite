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

### echos the install script into the temporary directory
function makeInstallScript()
{
echo "#!/bin/bash

VES_INSTALL_PREFIX=\"\"

function usage()
{
    echo \"usage: \$0 -p <prefix> [ -r ]\"
}

function postinstall()
{
    SETVAR_COMMAND=\"\"
    SETVAR_DELIMITER=\"\"
    prefix=\$1
    place_env_file_at_install_root=\$2
    env_file_dir=\$HOME

    if [ \$place_env_file_at_install_root = 1 ]
    then
        env_file_dir=\$prefix
    fi

    env_file_path=\$env_file_dir/VE-SuiteEnv
     
    if [ \$SHELL = \"/bin/bash\" ]
    then
        SETVAR_COMMAND=\"export\"
        SETVAR_DELIMITER=\"=\"
    else
        SETVAR_COMMAND=\"setenv\"
        SETVAR_DELIMITER=\" \"
    fi

    if [ -e \$env_file_path ]
    then
        rm \$env_file_path
    fi

    # set up VES_PREFIX
    var_assign_string=\"\$SETVAR_COMMAND VES_PREFIX\$SETVAR_DELIMITER\"
    var_value_string=\$prefix

    echo \$var_assign_string\$var_value_string >> \$env_file_path

    # set up PATH
    var_assign_string=\"\$SETVAR_COMMAND PATH\$SETVAR_DELIMITER\"
    var_value_string='\$VES_PREFIX/bin:\$PATH'

    echo \$var_assign_string\$var_value_string >> \$env_file_path

    # set up LD_LIBRARY_PATH
    var_assign_string=\"\$SETVAR_COMMAND LD_LIBRARY_PATH\$SETVAR_DELIMITER\"
    if [ -n \"\$LD_LIBRARY_PATH\" ]
    then
        var_value_string='\$VES_PREFIX/lib:\$VES_PREFIX/lib/vtk-5.8:\$VES_PREFIX/lib64:\$LD_LIBRARY_PATH'
    else
        var_value_string='\$VES_PREFIX/lib:\$VES_PREFIX/lib/vtk-5.8:\$VES_PREFIX/lib64'
    fi

    echo \$var_assign_string\$var_value_string >> \$env_file_path

    # set up OSG_FILE_PATH
    var_assign_string=\"\$SETVAR_COMMAND OSG_FILE_PATH\$SETVAR_DELIMITER\"
    var_value_string='\$VES_PREFIX/share/osgBullet/data:\$VES_PREFIX/share/osgWorks/data:\$VES_PREFIX/share/backdropFX/data'

    echo \$var_assign_string\$var_value_string >> \$env_file_path

    # set up VJ_BASE_DIR
    if [ -n \"\$VJ_BASE_DIR\" ]
    then
        var_assign_string=\"\$SETVAR_COMMAND VJ_BASE_DIR\$SETVAR_DELIMITER\"
        var_value_string='\$VES_PREFIX'

        echo \$var_assign_string\$var_value_string >> \$env_file_path
    fi

    # set up OSGNOTIFYLEVEL
    var_assign_string=\"\$SETVAR_COMMAND OSGNOTIFYLEVEL\$SETVAR_DELIMITER\"
    var_value_string='WARNING'

    echo \$var_assign_string\$var_value_string >> \$env_file_path

    # set up PYTHONPATH, if necessary
    current_working_dir=\$(pwd)
    cd \$1
    wxpython_base_dir=\$(find lib64 -type d -name \"python*\")
    if [ -n \"\$wxpython_base_dir\" ]
    then
        var_assign_string=\"\$SETVAR_COMMAND PYTHONPATH\$SETVAR_DELIMITER\"
        if [ -n \"\$PYTHONPATH\" ]
        then
            var_value_string='/site-packages/wx-2.8-gtk2-unicode:\$PYTHONPATH'
        else
            var_value_string='/site-packages/wx-2.8-gtk2-unicode'
        fi
        ves_prefix_string='\$VES_PREFIX/'

        echo \$var_assign_string\$ves_prefix_string\$wxpython_base_dir\$var_value_string >> \$env_file_path
    fi
    cd \$current_working_dir

    echo \"\"
    echo \"  A file to aid setting up the VE-Suite environment has been written to \$env_file_path.\"
    echo \"  Source this file in your shell's rc file.\"
}

place_env_file_at_install_root=0

while getopts \"p:r\" SCRIPT_ARGS
do
    case \$SCRIPT_ARGS in
    p)
        VES_INSTALL_PREFIX=\$OPTARG
        ;;
    r)
        place_env_file_at_install_root=1
        ;;
    ?)
        usage
        exit 1
    esac
done

if [ -z \$VES_INSTALL_PREFIX ]
then
    usage
    exit 1
fi

if [ -d \"\$VES_INSTALL_PREFIX\" ]
then
    if [ \"\$(ls -A \$VES_INSTALL_PREFIX)\" ]
    then
        echo \"ERROR! \$VES_INSTALL_PREFIX is not empty. Bailing out...\"
        exit 1
    fi
else
    echo \"\$VES_INSTALL_PREFIX does not exist. Creating it...\"
    mkdir -p \$VES_INSTALL_PREFIX
fi

# Install VE-Suite
echo \"Installing...\"
echo \"...Stage 1...\"
tar xf ./ve-suite.tar -C \$VES_INSTALL_PREFIX

echo \"...Stage 2...\"
tar xf ./ve-suite-deps.tar -C \$VES_INSTALL_PREFIX

if [ \"\$(ls -A extra/)\" ]
then
    echo \"...Stage 3...\"
    cd extra
    for extra_tar in *.tar
    do
        tar xf ./\$extra_tar -C \$VES_INSTALL_PREFIX
    done
fi

echo \"Finished!\"
postinstall \$VES_INSTALL_PREFIX \$place_env_file_at_install_root

cd .." > $INSTALLER_PAYLOAD_DIR/install

chmod +x $INSTALLER_PAYLOAD_DIR/install
}
###

### echoes the decompress script into the temporary directory
function makeDecompressScript()
{
echo "#!/bin/bash

function usage()
{
echo \"
  Usage: \$0 -p <prefix> [ -r ]

  OPTIONS:
   -p <prefix> : the full path to the VE-Suite install directory (REQUIRED)
   -r          : if this flag is specified, place the environment file
                 at the root of the VE-Suite install instead of your
                 home directory (OPTIONAL)
\"
}

INSTALL_PREFIX=\"\"
place_env_file_at_install_root=0

while getopts \"p:r\" SCRIPT_ARGS
do
    case \$SCRIPT_ARGS in
    p)
        INSTALL_PREFIX=\$OPTARG
        ;;
    r)
        place_env_file_at_install_root=1
        ;;
    ?)
        usage
        exit 1
        ;;
    esac
done

if [ -z \$INSTALL_PREFIX ]
then
    usage
    exit 1
fi

echo \"Decompressing...\"

DECOMPRESS_DIR=\`mktemp -d /tmp/ves-installer-decompress-dir.XXXXXX\`
ARCHIVE=\`awk '/^__ARCHIVE_BELOW_THIS_LINE__/ { print NR + 1; exit 0; }' \$0\`

tail -n+\$ARCHIVE \$0 | tar xj -C \$DECOMPRESS_DIR

echo \"Finished decompressing\"

CURRENT_WORKING_DIR=\`pwd\`
cd \$DECOMPRESS_DIR
if [ \$place_env_file_at_install_root = 1 ]
then
    ./install -p \$INSTALL_PREFIX -r
else
    ./install -p \$INSTALL_PREFIX
fi

cd \$CURRENT_WORKING_DIR
rm -rf \$DECOMPRESS_DIR

exit 0

__ARCHIVE_BELOW_THIS_LINE__" > $INSTALLER_ROOT_DIR/decompress

chmod +x $INSTALLER_ROOT_DIR/decompress
}

VES_INSTALL_PREFIX=""
VES_DEPS_TAR_FILE=""
VES_SOURCE_TREE_DIR=""
EXTRA_PAYLOAD_TAR_FILES=()
extra_filename_string=""

while getopts "p:d:t:x:s:" SCRIPT_ARGS
do
    case $SCRIPT_ARGS in
    p)
        VES_INSTALL_PREFIX=$OPTARG
        ;;
    d)
        VES_DEPS_TAR_FILE=$OPTARG
        ;;
    t)
        VES_SOURCE_TREE_DIR=$OPTARG
        ;;
    x)
        EXTRA_PAYLOAD_TAR_FILES=( ${EXTRA_PAYLOAD_TAR_FILES[@]} $OPTARG )
        ;;
    s)
        extra_filename_string=$OPTARG
        ;;
    ?)
        usage
        exit 1
        ;;
    esac
done

if [ -z $VES_INSTALL_PREFIX ]
then
    usage
    exit 1
fi

if [ -z $VES_DEPS_TAR_FILE ]
then
    usage
    exit 1
fi

if [ -z $VES_SOURCE_TREE_DIR ]
then
    usage
    exit 1
fi

INSTALLER_ROOT_DIR=`mktemp -d /tmp/ves-installer-rootdir.XXXXXX`
INSTALLER_PAYLOAD_DIR="$INSTALLER_ROOT_DIR/installer/payload"
INSTALLER_EXTRA_PAYLOAD_DIR="$INSTALLER_PAYLOAD_DIR/extra"

if [ ! -e "$VES_INSTALL_PREFIX/include/ves/VEConfig.h" ]
then
    echo "ERROR! Can't find VEConfig.h, bailing out!"
    rm -rf $INSTALLER_ROOT_DIR
    exit 1
fi

ves_major_version=$(awk '/VES_MAJOR_VERSION/ {print $3;}' $VES_INSTALL_PREFIX/include/ves/VEConfig.h)
ves_minor_version=$(awk '/VES_MINOR_VERSION/ {print $3;}' $VES_INSTALL_PREFIX/include/ves/VEConfig.h)
ves_patch_version=$(awk '/VES_PATCH_VERSION/ {print $3;}' $VES_INSTALL_PREFIX/include/ves/VEConfig.h)

svn_revision_number=$(svnversion $VES_SOURCE_TREE_DIR)

if [[ "$svn_revision_number" == *:* ]]
then
    echo "ERROR!. svnversion reports that your working copy at"
    echo "$VES_SOURCE_TREE_DIR is a mixed-revision working copy."
    echo "Update your working copy and try again."
    rm -rf $INSTALLER_ROOT_DIR
    exit 1
fi 

if [ ! -z "$extra_filename_string" ]
then
    # if the string is not empty, prepend a hyphen
    extra_filename_string="-$extra_filename_string"
fi

installer_file_name="VE-SuiteInstall-$ves_major_version.$ves_minor_version.$ves_patch_version-r$svn_revision_number$extra_filename_string.bash"

echo "Creating temporary directory structure at $INSTALLER_ROOT_DIR..."
mkdir -p $INSTALLER_EXTRA_PAYLOAD_DIR

CURRENT_WORKING_DIR=`pwd`

echo "Adding install script..."
makeInstallScript

echo "Adding decompress script..."
makeDecompressScript

# tar up the VE-Suite install and add it to the payload directory
echo "Archiving the VE-Suite install..."
cd $VES_INSTALL_PREFIX
tar cf $INSTALLER_PAYLOAD_DIR/ve-suite.tar bin/ lib64/ share/

# add the dependencies
echo "Adding dependencies..."
cp $VES_DEPS_TAR_FILE $INSTALLER_PAYLOAD_DIR/ve-suite-deps.tar

# any any extra tar files to the payload
for tarfile in ${EXTRA_PAYLOAD_TAR_FILES[@]}
do
    echo "Adding extra archive $tarfile to payload..."
    cp $tarfile $INSTALLER_EXTRA_PAYLOAD_DIR
done

echo "Archiving the payload..."
cd $INSTALLER_PAYLOAD_DIR
tar cf ../payload.tar ./*
cd ..

if [ -e "payload.tar" ]
then
    echo "Compressing the payload..."
    bzip2 payload.tar

    if [ -e "payload.tar.bz2" ]
    then
        echo "Creating the self-extracting installer..."
        cat $INSTALLER_ROOT_DIR/decompress payload.tar.bz2 > $CURRENT_WORKING_DIR/$installer_file_name
        chmod +x $CURRENT_WORKING_DIR/$installer_file_name
    else
        echo "payload.tar.bz2 does not exist!"
        exit 1
    fi
else
    echo "payload.tar does not exist!"
    exit 1
fi

echo "VE-Suite self-extracting installer created!"

echo "Cleaning up..."
rm -rf $INSTALLER_ROOT_DIR

exit 0
