#!/bin/sh
# this is a bourne shell script
# sets up environment to build and/or run VE-Xplorer

#this is typically defined in your .cshrc file
export VE_SUITE_HOME=/Volumes/data/TSVEG/VE_Suite

if [ -e /etc/redhat-release ]; then
   #echo "found /etc/redhat-release"
   # extract some words from file to create something like RedHat_8.0
   export CFDHOSTTYPE=`cat /etc/redhat-release | awk -F" " '{print $1 $2 "_" $5}'`
elif [ -e /etc/SuSE-release ]; then
   #echo "found /etc/SuSE-release"
   # extract first and third words from file to create something like SuSE_9.1
   export CFDHOSTTYPE=`head -1 /etc/SuSE-release | awk -F" " '{print $1 "_" $3}'`
else
   #echo "uname is" `uname`
   export CFDHOSTTYPE=`uname`
fi

#if creation of CFDHOSTTYPE caused parenthesis to be inserted, then remove...
export CFDHOSTTYPE=`echo \"$CFDHOSTTYPE\" | sed -e 's/(//g' | sed -e 's/)//g' | sed -e 's/"//g'`
#echo "CFDHOSTTYPE =" $CFDHOSTTYPE

export TAO_BUILD=TRUE
export CLUSTER_APP=FALSE
export SCENE_GRAPH=OSG
export VE_PATENTED=TRUE
#export VEXMASTER=ids7
#FLAGPOLL_PATH

export PFNFYLEVEL=2
#export VPR_DEBUG_NFY_LEVEL=1
#export VPR_DEBUG_ENABLE=1
#export VPR_DEBUG_ALLOW_CATEGORIES="VES_DBG"
#export VPR_DEBUG_DISALLOW_CATEGORIES ="VES_DBG DBG_KERNEL"
export NO_RTRC_PLUGIN=TRUE
export NO_PERF_PLUGIN=TRUE
export OSG_THREAD_SAFE_REF_UNREF=1
#export OSGNOTIFYLEVEL=DEBUG_INFO
#export OSGNOTIFYLEVEL=
#export PFSHAREDSIZE=534773700
#export OMNIORB_CONFIG=${VE_SUITE_HOME}/VE_Installer/omniORB4.cfg
#export OMNINAMES_LOGDIR=${VE_SUITE_HOME}/VE_Installer
#export DYLD_INSERT_LIBRARIES=/usr/lib/libMallocDebug.A.dylib
#:/usr/lib/libgmalloc.dylib
case "$CFDHOSTTYPE" in
   IRIX*) 
   #echo "CFDHOSTTYPE contains IRIX"
   export JDK_HOME=/usr/java2
   export VTK_BASE_DIR=/home/users/mccdo/vtk-builds/IRIX32
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
   export VTK_BASE_DIR=/home/users/sjk60/vtk/VTK-4.4/RedHat_8.0

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
   export JDK_HOME=/usr/lib/j2sdk1.4.2_03
   export VTK_BASE_DIR=/usr/local/vtk-4.4

   export VJ_BASE_DIR=/nfs/scratch/VE_Suite_Libs/vrjuggler-2.0.0
   export VJ_DEPS_DIR=/nfs/scratch/VE_Suite_Libs/vrjuggler-2.0.0-deps

   export LD_LIBRARY_PATH=${VJ_BASE_DIR}/lib:${VTK_BASE_DIR}/lib/vtk:${VJ_DEPS_DIR}/lib
   export WX_HOME=/nfs/scratch/Juggler_Deps/wxGTK-2.6.1/install-suse92
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${WX_HOME}/lib
   export OSG_HOME=/nfs/scratch/VE_Suite_Libs/OSG-0.9.9
   export LD_LIBRARY_PATH=${OSG_HOME}/lib:${OSG_HOME}/lib/osgPlugins:${LD_LIBRARY_PATH}
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${VE_SUITE_HOME}/lib/${CFDHOSTTYPE}

   export ACE_HOME=/nfs/scratch/ACE_TAO/ACE-5.4
   export TAO_HOME=/nfs/scratch/ACE_TAO/TAO-1.4
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${ACE_HOME}/Linux/lib
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${TAO_HOME}/Linux/lib
   export PATH=${ACE_HOME}/Linux/bin:${TAO_HOME}/Linux/bin:${PATH}

   export XERCESCROOT=/nfs/scratch/xerces
   export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${XERCESCROOT}/lib

;;
	Darwin*)
   export FLAGPOLL_PATH=${VE_SUITE_HOME}/VE_Installer/fpc:/Volumes/data/VE_Suite_Deps/vrjuggler-2.0-svn/install-darwin/lib/flagpoll
   export FLAGPOLL_PATH=${FLAGPOLL_PATH}:/Volumes/data/VE_Suite_Deps/vrjuggler/cppdom-0.6.6/lib/flagpoll
   export FLAGPOLL_PATH=${FLAGPOLL_PATH}:/Volumes/data/VE_Suite_Deps/ACE_TAO/install/lib/pkgconfig
   export FLAGPOLL_PATH=${FLAGPOLL_PATH}:/Volumes/data/VE_Suite_Deps/bullet-2.43/install-darwin/lib/pkgconfig:/opt/local/share/pkgconfig

   #setenv JDK_HOME /usr/java
   export VTK_BASE_DIR=/Volumes/data/VE_Suite_Deps/vtk-cvs/install
   export TAO_HOME=/Volumes/data/VE_Suite_Deps/ACE_TAO/install
   #export WX_HOME=/Volumes/data/VE_Suite_Deps/wxWidgets/install
   export VJ_BASE_DIR=/Volumes/data/VE_Suite_Deps/vrjuggler-2.0-svn/install-darwin
   export OSG_HOME=/Volumes/data/VE_Suite_Deps/OSG_OP_OT-1.2/install
   #setenv CORONA_HOME /home/vr/Applications/TSVEG/Libraries/Release/Opt/corona-1.0.2/Linux-SuSE92

   export DYLD_LIBRARY_PATH=${VJ_BASE_DIR}/lib:${VTK_BASE_DIR}/lib
   export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ImageIO.framework/Versions/A/Resources
   export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${OSG_HOME}/lib:${OSG_HOME}/lib/osgPlugins
   #export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${WX_HOME}/lib
   export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${VE_SUITE_HOME}/test_two/lib
   export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${TAO_HOME}/lib
   export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:/Volumes/data/VE_Suite_Deps/opal-install/lib
   export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:/opt/local/lib:/Volumes/data/VE_Suite_Deps/bullet-2.43/install-darwin/lib

   export PATH=${TAO_HOME}/bin:${PATH}

;;
   *)
   echo "ERROR: Unsupported operating system"
   echo "       OMNI_HOME, etc. are undefined"
;;
esac

export DZR_BASE_DIR=${VJ_BASE_DIR}/share/Doozer
export PATH=${VJ_BASE_DIR}/bin:${VE_SUITE_HOME}/test_two/bin:${PATH}
#export PATH=${WX_HOME}/bin:${PATH}

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
