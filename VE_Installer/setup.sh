#!/bin/sh
# this is a bourne shell script
# sets up environment to build and/or run VE-Xplorer

#OMNI_HOME: comment out definitions to make a non-corba version - that's all you have to do

#this is typically defined in your .cshrc file
#VE_SUITE_HOME=/home/vr/Applications/TSVEG/VE_Suite

if [ -e /etc/redhat-release ]; then
   #echo "found /etc/redhat-release"
   # extract some words from file to create something like RedHat_8.0
   export CFDHOSTTYPE=`cat /etc/redhat-release | awk -F" " '{print $1 $2 "_" $5}'`
elif [ -e /etc/SuSE-release ]; then
   #echo "found /etc/SuSE-release"
   # extract first and third words from file to create something like SuSE_9.1
   export CFDHOSTTYPE=`head -1 /etc/SuSE-release | awk -F" " '{print $1 "_" $3}'`
else
   echo "uname is" `uname`
   export CFDHOSTTYPE=`uname`
fi

echo "CFDHOSTTYPE =" $CFDHOSTTYPE

#export TAO_BUILD=TRUE
export TAO_BUILD=FALSE

export PFNFYLEVEL=2
export VPR_DEBUG_NFY_LEVEL=0
export VPR_DEBUG_ENABLE=1
export PFSHAREDSIZE=534773700
export WX_HOME_DIR=/home/users/mccdo/wxWindows/wxGTK
export OMNIORB_CONFIG=${VE_SUITE_HOME}/VE_Installer/omniORB4.cfg
export OMNINAMES_LOGDIR=${VE_SUITE_HOME}/VE_Installer
VTK_HOME_DIR=/home/users/mccdo/vtk-builds

case "$CFDHOSTTYPE" in
   IRIX*) 
   #echo "CFDHOSTTYPE contains IRIX"
   export JDK_HOME=/usr/java2
   export VTK_BASE_DIR=${VTK_HOME_DIR}/IRIX32
   export VJ_BASE_DIR=/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-pthread
   export VJ_DEPS_DIR=/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-deps
#   export LD_LIBRARYN32_PATH=/home/users/mccdo/VE_Suite/VE_Installer/arenasize/libs
   export LD_LIBRARYN32_PATH=${VJ_BASE_DIR}/lib32:${VTK_BASE_DIR}/lib/vtk:${VJ_DEPS_DIR}/lib32
   export LD_LIBRARYN32_PATH=${LD_LIBRARYN32_PATH}:/home/users/jhynek/Pigs/Oinks/OpenAL/openal/linux/bin/lib
   export LD_LIBRARYN32_PATH=${LD_LIBRARYN32_PATH}:/home/users/mccdo/software/IRIX32/lib
   export WX_HOME=${WX_HOME_DIR}/irix-65
   export BOOST_INCLUDES=${VJ_DEPS_DIR}/include

   if [ ${TAO_BUILD} = "TRUE" ]; then
      export ACE_ROOT=/home/users/mccdo/ACE_TAO/Irix-vrac/ACE_wrappers
      export TAO_ROOT=${ACE_ROOT}/TAO
      export LD_LIBRARYN32_PATH=${LD_LIBRARYN32_PATH}:${ACE_ROOT}/ace:${ACE_ROOT}/lib
      export LD_LIBRARYN32_PATH=${LD_LIBRARYN32_PATH}:${TAO_ROOT}/TAO_IDL:${WX_HOME}/lib
   else
#      export OMNI_HOME=/home/vr/Juggler/irix/mipspro-omniORB-4.0.1
#      export OMNI_HOME=/home/users/mccdo/software/IRIX32
      export OMNI_HOME=${VJ_DEPS_DIR}
#/home/vr/Juggler/irix/mipspro
      export PYTHONPATH=${OMNI_HOME}/lib/python1.5/site-packages
      export LD_LIBRARYN32_PATH=${LD_LIBRARYN32_PATH}:${OMNI_HOME}/lib:${WX_HOME}/lib
   fi
;;
   RedHat*) 
   #echo "CFDHOSTTYPE contains RedHat"
   export VTK_BASE_DIR=${VTK_HOME_DIR}/Linux-rh

   export JDK_HOME=/usr/java/j2sdk1.4.2_03
   export VJ_BASE_DIR=/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.linux-rh80
   export VJ_DEPS_DIR=/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.linux-rh80-deps

   export BOOST_INCLUDES=${VJ_DEPS_DIR}/include
   export LD_LIBRARY_PATH=${VJ_BASE_DIR}/lib:${VTK_BASE_DIR}/lib/vtk:${VJ_DEPS_DIR}/lib
   export WX_HOME=${WX_HOME_DIR}/linux-rh80

   if [ ${TAO_BUILD} = "TRUE" ]; then
      export WX_HOME=${WX_HOME_DIR}/linux-rh80
      export ACE_ROOT=/home/users/mccdo/ACE_TAO/Suse-9-vrac/ACE_wrappers
      #export ACE_ROOT=/home/users/mccdo/ACE_TAO/Linux-rh80-vrac/ACE_wrappers

      export TAO_ROOT=${ACE_ROOT}/TAO
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${ACE_ROOT}/ace:${ACE_ROOT}/lib
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${TAO_ROOT}/TAO_IDL:${WX_HOME}/lib
   else
      export OMNI_HOME=/home/vr/Juggler/linux-rh80
      #export OMNI_HOME=/home/vr/Juggler/linux-fc1
      export PYTHONPATH=${OMNI_HOME}/lib/python2.2/site-packages
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${OMNI_HOME}/lib:${WX_HOME}/lib
   fi
;;
   SuSE*) 
   #echo "CFDHOSTTYPE contains SuSE"
   export VTK_BASE_DIR=${VTK_HOME_DIR}/Linux-rh8

   export JDK_HOME=/usr/lib/java2
   export VJ_BASE_DIR=/home/users/mccdo/vrjuggler-builds/Suse-9.1-alpha4
   export VJ_DEPS_DIR=/home/users/mccdo/cppdom-0.3.2/Suse-9.1

   export BOOST_INCLUDES=${VJ_DEPS_DIR}/include/boost-1_31
   export LD_LIBRARY_PATH=${VJ_BASE_DIR}/lib:${VTK_BASE_DIR}/lib/vtk:${VJ_DEPS_DIR}/lib
   export WX_HOME=${WX_HOME_DIR}/linux-rh80

   if [ ${TAO_BUILD} = "TRUE" ]; then
      export WX_HOME=${WX_HOME_DIR}/linux-rh80
      export ACE_ROOT=/home/users/mccdo/ACE_TAO/Suse-9-vrac/ACE_wrappers
      #export ACE_ROOT=/home/users/mccdo/ACE_TAO/Linux-rh80-vrac/ACE_wrappers

      export TAO_ROOT=${ACE_ROOT}/TAO
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${ACE_ROOT}/ace:${ACE_ROOT}/lib
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${TAO_ROOT}/TAO_IDL:${WX_HOME}/lib
   else
      export OMNI_HOME=/home/vr/Juggler/linux-rh80
      #export OMNI_HOME=/home/vr/Juggler/linux-fc1
      export PYTHONPATH=${OMNI_HOME}/lib/python2.2/site-packages
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${OMNI_HOME}/lib:${WX_HOME}/lib
   fi
;;
   *)
   echo "ERROR: Unsupported operating system"
   echo "       OMNI_HOME, etc. are undefined"
;;
esac

export TWEEK_BASE_DIR=${VJ_BASE_DIR}
export DZR_BASE_DIR=${VJ_BASE_DIR}/share/Doozer
export SNX_BASE_DIR=${VJ_BASE_DIR}
export PATH=${PATH}:${VJ_BASE_DIR}/bin:${VE_SUITE_HOME}/bin:${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}:${VJ_DEPS_DIR}/bin
export PATH=${WX_HOME}/bin:${PATH}
export PATH=${JDK_HOME}/bin:${PATH}
if [ ${TAO_BUILD} = "TRUE" ]; then
   export PATH=${ACE_ROOT}/bin:${PATH}
else
   export PATH=${OMNI_HOME}/bin:${PATH}
fi

#echo ""
#echo "Now you may type 'gmake' to build the application"
#echo "              or 'gmake clean'
#echo "              or 'gmake cleandepend'
#echo "              or 'run' to start the application in sim mode"
#echo "              or 'runc6' to start the application on the c6"
#echo "              or 'runc4.closed' or 'runc4.open' to start the application on the c4"
#echo ""
