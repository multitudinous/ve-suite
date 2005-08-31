#!/bin/sh
# this is a bourne shell script
# sets up environment to build and/or run VE-Xplorer

#this is typically defined in your .cshrc file
#VE_SUITE_HOME=/home/vr/Applications/TSVEG/VE_Suite

if [[ -e /etc/redhat-release ]]
then
   #echo "found /etc/redhat-release"
   # extract some words from file to create something like RedHat_8.0
   #export CFDHOSTTYPE=`cat /etc/redhat-release | awk -F" " '{print $1 $2 "_" $5}'`
   export firstWord=`cat /etc/redhat-release | awk -F" " '{print $1}'`
   #echo "firstWord is" ${firstWord}
   if [[ ${firstWord} = "Red" ]]
   then
      export thirdWord=`cat /etc/redhat-release | awk -F" " '{print $3}'`
      if [[ ${thirdWord} = "Enterprise" ]]
      then
         # extract some words from file to create something like RHEL_3
         export CFDHOSTTYPE=`cat /etc/redhat-release | awk -F" " '{print "RHEL_" $7}'`
      else
         # extract some words from file to create something like RedHat_8.0 or RedHat_9
         export CFDHOSTTYPE=`cat /etc/redhat-release | awk -F" " '{print $1 $2 "_" $5}'`
      fi
   elif [[ ${firstWord} = "Fedora" ]]
   then
      # extract some words from file to create something like Fedora_1
      export CFDHOSTTYPE=`cat /etc/redhat-release | awk -F" " '{print $1 "_" $4}'`
   fi
elif [[ -e /etc/SuSE-release ]]
then
   #echo "found /etc/SuSE-release"
   # extract first and third words from file to create something like SuSE_9.1
   export CFDHOSTTYPE=`head -1 /etc/SuSE-release | awk -F" " '{print $1 "_" $3 "_" $4}'`
else
   echo "uname is" `uname`
   export CFDHOSTTYPE=`uname`
fi

#if creation of CFDHOSTTYPE caused parenthesis to be inserted, then remove...
export CFDHOSTTYPE=`echo \"$CFDHOSTTYPE\" | sed -e 's/(//g' | sed -e 's/)//g' | sed -e 's/"//g'`

# Specify your scene graph: OSG = OpenSceneGraph, PF = OpenGL Performer
export SCENE_GRAPH=OSG 
#export SCENE_GRAPH=PF

# Specify corba build TAO_BUILD only for right now
export TAO_BUILD=TRUE

# Specify cluster build CLUSTER_APP
#export CLUSTER_APP=TRUE
# This flag is for VERY new code
#export VE_PATENTED=TRUE
# This flag is to allow the use of shaders
#export CFD_USE_SHADERS=FALSE

# TAO_BUILD can be set outside this script to override the default of TRUE
# Make sure that TAO_BUILD is set to either TRUE or FALSE...
if [[ ! $?TAO_BUILD ]]
then
   export TAO_BUILD=TRUE
  #echo "   Because it was undefined, setting TAO_BUILD to" ${TAO_BUILD}
  #echo "   If you want otherwise, open new shell, define TAO_BUILD, and run script again"
  #echo ""
elif [[ ${TAO_BUILD} != "TRUE" && ${TAO_BUILD} != "FALSE" ]]
then
   export TAO_BUILD=TRUE
  #echo "   Because it was not defined properly, setting TAO_BUILD to" ${TAO_BUILD}
  #echo "   If you want otherwise, open new shell, define TAO_BUILD, and run script again"
  #echo ""
fi

# CLUSTER_APP can be set outside this script to override the default of FALSE
# Make sure that CLUSTER_APP is set to either TRUE or FALSE...
if [[ ! $?CLUSTER_APP ]]
then
   setenv CLUSTER_APP FALSE
  #echo "   Because it was undefined, setting CLUSTER_APP to" ${CLUSTER_APP}
  #echo "   If you want otherwise, redefine CLUSTER_APP"
elif [[ ${CLUSTER_APP} != "TRUE" && ${CLUSTER_APP} != "FALSE" ]]
then
   setenv CLUSTER_APP FALSE
   #echo "   Because it was not defined properly, setting CLUSTER_APP to" ${CLUSTER_APP}
   #echo "   If you want otherwise, redefine CLUSTER_APP and resource the setup file"
fi

export PFNFYLEVEL=0
export PFSHAREDSIZE=534773700
export VPR_DEBUG_NFY_LEVEL=1
export VPR_DEBUG_ENABLE=1
#export VPR_DEBUG_FILE=${VE_SUITE_HOME}/VE_Xplorer/VPRDebugOutput.txt
#export VPR_DEBUG_ALLOW_CATEGORIES="DBG_ALL DBG_ERROR VES_DBG"
#export VPR_DEBUG_DISALLOW_CATEGORIES="VES_DBG DBG_KERNEL"
export VPR_DEBUG_DISALLOW_CATEGORIES=
export NO_RTRC_PLUGIN=TRUE
export NO_PERF_PLUGIN=TRUE
export OSG_THREAD_SAFE_REF_UNREF=1
export OSGNOTIFYLEVEL=DEBUG_INFO
export OMNIORB_CONFIG=${VE_SUITE_HOME}/VE_Installer/omniORB4.cfg
export OMNINAMES_LOGDIR=${VE_SUITE_HOME}/VE_Installer

case "$CFDHOSTTYPE" in
   IRIX*) 
   #echo "CFDHOSTTYPE contains IRIX"
   export JDK_HOME=/usr/java2
   export VTK_BASE_DIR=/home/users/mccdo/vtk-builds/IRIX32
   export WX_HOME=${WX_HOME_DIR}/irix-65
   export VJ_BASE_DIR=/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-pthread
   export VJ_DEPS_DIR=/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-deps
   export OSG_HOME=/home/vr/Applications/TSVEG/Libraries/Release/Opt/OSG/IRIX32
   export CORONA_HOME=/home/vr/Applications/TSVEG/Libraries/Release/Opt/corona-1.0.2/Linux-SuSE92

   export LD_LIBRARYN32_PATH=${VJ_BASE_DIR}/lib32:${VTK_BASE_DIR}/lib/vtk:${VJ_DEPS_DIR}/lib32
   export LD_LIBRARYN32_PATH=${LD_LIBRARYN32_PATH}:${WX_HOME}/lib
   export LD_LIBRARYN32_PATH=${LD_LIBRARYN32_PATH}:${OSG_HOME}/lib32:${OSG_HOME}/lib32/osgPlugins
   export LD_LIBRARYN32_PATH=${LD_LIBRARYN32_PATH}:${VE_SUITE_HOME}/lib/${CFDHOSTTYPE}

   if [[ ${TAO_BUILD} = "TRUE" ]]
   then
      export ACE_HOME=/home/vr/Applications/TSVEG/Libraries/ACE-5.4
      export TAO_HOME=/home/vr/Applications/TSVEG/Libraries/TAO-1.4
      export LD_LIBRARYN32_PATH=${LD_LIBRARYN32_PATH}:${ACE_HOME}/Linux/lib
      export LD_LIBRARYN32_PATH=${LD_LIBRARYN32_PATH}:${TAO_HOME}/Linux/lib
      export PATH=${ACE_HOME}/bin:${ACE_HOME}/IRIX32/bin:${TAO_HOME}/IRIX32/bin:${PATH}
      
      export XERCESCROOT=/home/vr/Applications/TSVEG/Libraries/xerces-2.5/IRIX32
      export LD_LIBRARYN32_PATH=${LD_LIBRARYN32_PATH}:${XERCESCROOT}/lib
   else
      export PYTHONPATH=${OMNI_HOME}/lib/python2.2/site-packages
   fi
;;
   RedHat*|RHEL_*|Fedora_*) 
   #echo "CFDHOSTTYPE contains RedHat"
   export JDK_HOME=/usr/java2
   export VTK_BASE_DIR=/home/users/mccdo/vtk-builds/IRIX32
   export WX_HOME=${WX_HOME_DIR}/irix-65
   export VJ_BASE_DIR=/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-pthread
   export VJ_DEPS_DIR=/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-deps
   export OSG_HOME=/home/vr/Applications/TSVEG/Libraries/Release/Opt/OSG/IRIX32
   export CORONA_HOME=/home/vr/Applications/TSVEG/Libraries/Release/Opt/corona-1.0.2/Linux-SuSE92

   export LD_LIBRARY_PATH=${VJ_BASE_DIR}/lib32:${VTK_BASE_DIR}/lib/vtk:${VJ_DEPS_DIR}/lib32
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${WX_HOME}/lib
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${OSG_HOME}/lib32:${OSG_HOME}/lib32/osgPlugins
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${VE_SUITE_HOME}/lib/${CFDHOSTTYPE}

   if [[ ${TAO_BUILD} = "TRUE" ]] 
   then
      export ACE_HOME=/home/vr/Applications/TSVEG/Libraries/ACE-5.4
      export TAO_HOME=/home/vr/Applications/TSVEG/Libraries/TAO-1.4
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${ACE_HOME}/Linux/lib
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${TAO_HOME}/Linux/lib
      export PATH=${ACE_HOME}/bin:${ACE_HOME}/IRIX32/bin:${TAO_HOME}/IRIX32/bin:${PATH}
      
      export XERCESCROOT=/home/vr/Applications/TSVEG/Libraries/xerces-2.5/IRIX32
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${XERCESCROOT}/lib
   else
      export PYTHONPATH=${OMNI_HOME}/lib/python2.2/site-packages
   fi
;;
   SuSE*) 
   #echo "CFDHOSTTYPE contains SuSE"
   export JDK_HOME=/usr/lib/java2
   export VTK_BASE_DIR=/home/vr/Applications/TSVEG/Libraries/VTK4.4/Linux-SuSE91
   export WX_HOME=/home/vr/Applications/TSVEG/Libraries/wxGTK-2.4.2/Linux-SuSE91
   export VJ_BASE_DIR=/home/vr/Applications/TSVEG/Libraries/vrjuggler-2.0b2/Linux-SuSE91
   export VJ_DEPS_DIR=/home/vr/Applications/TSVEG/Libraries/vrjuggler-2.0b2-deps/Linux-SuSE91
   export OSG_HOME=/home/vr/Applications/TSVEG/Libraries/OSG/Linux-SuSE91

   export LD_LIBRARY_PATH=${VJ_BASE_DIR}/lib:${VTK_BASE_DIR}/lib/vtk:${VJ_DEPS_DIR}/lib
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${WX_HOME}/lib
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${OSG_HOME}/lib:${OSG_HOME}/lib/osgPlugins

   if [[ ${TAO_BUILD} = "TRUE" ]]
   then
      export MY_ACEDIR=/home/users/mccdo/ACE_TAO
      export ACE_ROOT=/home/users/mccdo/ACE_TAO/Suse-9-vrac/ACE_wrappers
      export TAO_ROOT=${ACE_ROOT}/TAO
      export ACE_HOME=/home/vr/Applications/TSVEG/Libraries/ACE-5.4
      export TAO_HOME=/home/vr/Applications/TSVEG/Libraries/TAO-1.4
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${ACE_HOME}/Linux/lib
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${TAO_HOME}/Linux/lib
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${ACE_ROOT}/ace:${ACE_ROOT}/lib
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${TAO_ROOT}/TAO_IDL
      export PATH=${ACE_ROOT}/bin:${PATH}

      export XERCESCROOT=/home/vr/Applications/TSVEG/Libraries/xerces-2.5/Linux-SuSE91
      export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${XERCESCROOT}/lib
   else
      export PYTHONPATH=${OMNI_HOME}/lib/python2.2/site-packages
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
if [[ $?OSG_HOME ]]
then
   export PATH=${OSG_HOME}/share/OpenSceneGraph/bin:${PATH}
fi
#echo ""
#echo "Now you may type 'gmake' to build the application"
#echo "              or 'gmake clean'
#echo "              or 'gmake cleandepend'
#echo "              or 'run' to start the application in sim mode"
#echo "              or 'runc6' to start the application on the c6"
#echo "              or 'runc4.closed' or 'runc4.open' to start the application on the c4"
#echo ""
