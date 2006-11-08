#!/bin/sh
# this is a bourne shell script
# sets up environment to build and/or run VE-Xplorer

#this is typically defined in your .cshrc file

#export VE_SUITE_HOME=/nfs/scratch/VE_Suite

if [ -e /etc/redhat-release ]; then
   #echo "found /etc/redhat-release"
   # extract some words from file to create something like RedHat_8.0
   export CFDHOSTTYPE=`cat /etc/redhat-release | awk -F" " '{print $1 $2 "_" $5}'`
elif [ -e /etc/SuSE-release ]; then
   #echo "found /etc/SuSE-release"
   # extract first and third words from file to create something like SuSE_9.1
   export CFDHOSTTYPE=`head -1 /etc/SuSE-release | awk -F" " '{print $1 "_" $3 "_" $4}'`
else
   #echo "uname is" `uname`
   export CFDHOSTTYPE=`uname`
fi

#if creation of CFDHOSTTYPE caused parenthesis to be inserted, then remove...
export CFDHOSTTYPE=`echo \"$CFDHOSTTYPE\" | sed -e 's/(//g' | sed -e 's/)//g' | sed -e 's/"//g'`
#echo "CFDHOSTTYPE =" $CFDHOSTTYPE

export TAO_BUILD=TRUE
export CLUSTER_APP=TRUE
export SCENE_GRAPH=OSG
export VE_PATENTED=TRUE
export VEXMASTER=ids7
#FLAGPOLL_PATH

export PFNFYLEVEL=2
export VPR_DEBUG_NFY_LEVEL=0
export VPR_DEBUG_ENABLE=1
#export VPR_DEBUG_ALLOW_CATEGORIES="VES_DBG"
#export VPR_DEBUG_DISALLOW_CATEGORIES ="VES_DBG DBG_KERNEL"
export NO_RTRC_PLUGIN=TRUE
export NO_PERF_PLUGIN=TRUE
export OSG_THREAD_SAFE_REF_UNREF=1
export OSGNOTIFYLEVEL=DEBUG_INFO
#export OSGNOTIFYLEVEL=
export PFSHAREDSIZE=534773700
export OMNIORB_CONFIG=${VE_SUITE_HOME}/VE_Installer/omniORB4.cfg
export OMNINAMES_LOGDIR=${VE_SUITE_HOME}/VE_Installer
#export DYLD_INSERT_LIBRARIES=/usr/lib/libMallocDebug.A.dylib
   #echo "CFDHOSTTYPE contains SuSE"
   export JDK_HOME=/etc/alternatives/java_sdk_1.4.2
   export VTK_BASE_DIR=/nfs/scratch/VE_Suite_Libs/VTK-5.0

   export VJ_BASE_DIR=/usr/local/juggler
   export VJ_DEPS_DIR=/usr/local/juggler-deps

   export LD_LIBRARY_PATH=${VJ_BASE_DIR}/lib:${VTK_BASE_DIR}/lib/vtk:${VJ_DEPS_DIR}/lib:${VTK_BASE_DIR}/lib
   export WX_HOME=/nfs/scratch/wxGTK-2.6.3/install
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${WX_HOME}/lib
   export OSG_HOME=/nfs/scratch/OSG_OP_OT-1.2/install
   export LD_LIBRARY_PATH=${OSG_HOME}/lib:${OSG_HOME}/lib/osgPlugins:${LD_LIBRARY_PATH}
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${VE_SUITE_HOME}/lib/${CFDHOSTTYPE}

   export ACE_HOME=/nfs/scratch/ACE-5.5_TAO-1.5/install
   export TAO_HOME=/nfs/scratch/ACE-5.5_TAO-1.5/install
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${ACE_HOME}/lib
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${TAO_HOME}/lib
   export PATH=${ACE_HOME}/bin:${TAO_HOME/bin}:${PATH}

   export XERCESCROOT=/nfs/scratch/xerces-c-src_2_7_0
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${XERCESCROOT}/lib

	export PYTHONPATH=/nfs/scratch/VE_Suite_Libs/wxPython-src-2.6.3.3/wxPython
	

export TWEEK_BASE_DIR=${VJ_BASE_DIR}
export DZR_BASE_DIR=${VJ_BASE_DIR}/share/Doozer
export SNX_BASE_DIR=${VJ_BASE_DIR}
export PATH=${VJ_BASE_DIR}/bin:${VE_SUITE_HOME}/bin:${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}:${VJ_DEPS_DIR}/bin:${PATH}
export PATH=${WX_HOME}/bin:${PATH}
export LD_LIBRARY_PATH=/nfs/scratch/VE_Suite_Libs/wxPython-src-2.6.3.3/install-suse/lib:${LD_LIBRARY_PATH}
if [ $OSG_HOME ]; then
   export PATH=${OSG_HOME}/share/OpenSceneGraph/bin:${PATH}
   export OSG_FILE_PATH=${OSG_HOME}/share/OpenSceneGraph-Data
fi

#echo ""
#echo "Now you may type 'gmake' to build the application"
#echo "              or 'gmake clean'
#echo "              or 'gmake cleandepend'
#echo "              or 'run' to start the application in sim mode"
#echo "              or 'runc6' to start the application on the c6"
#echo "              or 'runc4.closed' or 'runc4.open' to start the application on the c4"
#echo ""
