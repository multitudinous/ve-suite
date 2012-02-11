#!/bin/bash

function usage()
{
echo "
  Usage: $0 -p <VE-Suite install prefix> -d <deps tar file> [ -x <extra tar file> ]

    Options:

    -p  The directory where VE-Suite is installed (REQUIRED)
    -d  The path to a tar file of VE-Suite's dependencies (REQUIRED)
    -x  Path to a tar file to add to the installer's payload (OPTIONAL)
        NOTE: The -x flag may be used as many times as is necessary"
}

### echos the install script into the temporary directory
function makeInstallScript()
{
echo "#!/bin/bash

VES_INSTALL_PREFIX=\"\"

function usage()
{
    echo \"usage: \$0 -p <prefix>\"
}

function postinstall()
{
    if [ \$SHELL = \"/bin/bash\" ]
    then
        echo \"export PATH=\$VES_INSTALL_PREFIX/bin:\$PATH\" >> \$HOME/VE-Suite_Env
        echo \"export LD_LIBRARY_PATH=\$VES_INSTALL_PREFIX/lib:\$VES_INSTALL_PREFIX/lib/vtk-5.8:\$VES_INSTALL_PREFIX/lib64:\$LD_LIBRARY_PATH\" >> \$HOME/VE-Suite_Env
    else
        echo \"setenv PATH=\$VES_INSTALL_PREFIX/bin:\$PATH\" >> \$HOME/VE-Suite_Env
        echo \"setenv LD_LIBRARY_PATH=\$VES_INSTALL_PREFIX/lib:\$VES_INSTALL_PREFIX/lib/vtk-5.8:\$VES_INSTALL_PREFIX/lib64:\$LD_LIBRARY_PATH\" >> \$HOME/VE-Suite_Env
    fi

    echo \"Required environment variables have been written to \$HOME/VE-Suite_Env.\"
    echo \"Source this file in your shell's rc file.\"
}

while getopts \"p:\" SCRIPT_ARGS
do
    case \$SCRIPT_ARGS in
    p)
        VES_INSTALL_PREFIX=\$OPTARG
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

mkdir -p \$VES_INSTALL_PREFIX

# Install VE-Suite
echo \"Installing...\"
echo \"Unarchiving VE-Suite...\"
tar xf ./ve-suite.tar -C \$VES_INSTALL_PREFIX

echo \"Unarchiving VE-Suite dependencies...\"
tar xf ./ve-suite-deps.tar -C \$VES_INSTALL_PREFIX

cd extra
for extra_tar in *.tar
do
    echo \"Unarchiving \$extra_tar...\"
    tar xf ./\$extra_tar -C \$VES_INSTALL_PREFIX
done

echo \"Finished!\"
postinstall

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
  Usage: \$0 -p <prefix>\"
}

INSTALL_PREFIX=\"\"

while getopts \"p:\" SCRIPT_ARGS
do
    case \$SCRIPT_ARGS in
    p)
        INSTALL_PREFIX=\$OPTARG
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
./install -p \$INSTALL_PREFIX

cd \$CURRENT_WORKING_DIR
rm -rf \$DECOMPRESS_DIR

exit 0

__ARCHIVE_BELOW_THIS_LINE__" > $INSTALLER_ROOT_DIR/decompress

chmod +x $INSTALLER_ROOT_DIR/decompress
}

VES_INSTALL_PREFIX=""
VES_DEPS_TAR_FILE=""
EXTRA_PAYLOAD_TAR_FILES=()

while getopts "p:d:x:" SCRIPT_ARGS
do
    case $SCRIPT_ARGS in
    p)
        VES_INSTALL_PREFIX=$OPTARG
        ;;
    d)
        VES_DEPS_TAR_FILE=$OPTARG
        ;;
    x)
        EXTRA_PAYLOAD_TAR_FILES=( ${EXTRA_PAYLOAD_TAR_FILES[@]} $OPTARG )
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

INSTALLER_ROOT_DIR=`mktemp -d /tmp/ves-installer-rootdir.XXXXXX`
INSTALLER_PAYLOAD_DIR="$INSTALLER_ROOT_DIR/installer/payload"
INSTALLER_EXTRA_PAYLOAD_DIR="$INSTALLER_PAYLOAD_DIR/extra"

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
        cat $INSTALLER_ROOT_DIR/decompress payload.tar.bz2 > $CURRENT_WORKING_DIR/VE-SuiteInstaller.bash
        chmod +x $CURRENT_WORKING_DIR/VE-SuiteInstaller.bash
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