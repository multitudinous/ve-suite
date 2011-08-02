#!/bin/csh
# sets up environment to build and/or run VE-Xplorer

#this is typically defined in your .cshrc file
#setenv VE_SUITE_HOME /home/vr/Applications/TSVEG/VE_Suite

#setenv PFNFYLEVEL 0
#setenv PFSHAREDSIZE 534773700
setenv VPR_DEBUG_NFY_LEVEL 2
setenv VPR_DEBUG_ENABLE 1
#setenv VPR_DEBUG_FILE ${VE_SUITE_HOME}/VE_Xplorer/VPRDebugOutput.txt
#setenv VPR_DEBUG_ALLOW_CATEGORIES "VES_DBG"
#setenv VPR_DEBUG_DISALLOW_CATEGORIES "VES_DBG"
#setenv VPR_DEBUG_DISALLOW_CATEGORIES
setenv NO_RTRC_PLUGIN TRUE
setenv NO_PERF_PLUGIN TRUE
#setenv OSG_THREAD_SAFE_REF_UNREF 1
#setenv OSGNOTIFYLEVEL DEBUG_INFO

setenv JDK_HOME /usr/java/latest
setenv JAVA_HOME /usr/java/latest

setenv VES_DEV_DIR /home/users/mccdo/dev/deps
setenv ACETAO_INSTALL_DIR ${VES_DEV_DIR}/ACE_wrappers/install-64-bit
setenv BDFX_INSTALL_DIR ${VES_DEV_DIR}/bdfx/install-64-bit
setenv BOOST_INSTALL_DIR ${VES_DEV_DIR}/boost_1_46_1/install-64-bit
setenv BULLET_INSTALL_DIR ${VES_DEV_DIR}/bullet-2.77/install-64-bit
setenv CPPDOM_INSTALL_DIR ${VES_DEV_DIR}/cppdom-1.2.0/install-64-bit
setenv GMTL_INSTALL_DIR ${VES_DEV_DIR}/gmtl-0.6.1/install-64-bit
setenv JUGGLER_INSTALL_DIR ${VES_DEV_DIR}/vrjuggler-trunk/install-64-bit
setenv OSG_INSTALL_DIR ${VES_DEV_DIR}/osg_2.8.5/install-64-bit
setenv OSGBULLET_INSTALL_DIR ${VES_DEV_DIR}/osgBullet/install-64-bit
setenv OSGBULLETPLUS_INSTALL_DIR ${VES_DEV_DIR}/osgBulletPlus/install-64-bit
setenv OSGEPHEMERIS_INSTALL_DIR ${VES_DEV_DIR}/osgEphemeris/install-64-bit
setenv OSGWORKS_INSTALL_DIR ${VES_DEV_DIR}/osgWorks/install-64-bit
setenv PNG_INSTALL_DIR ${VES_DEV_DIR}/libpng-1.5.2/install-64-bit
setenv POCO_INSTALL_DIR ${VES_DEV_DIR}/poco-1.4.1p1-all/install-64-bit
setenv VTK_INSTALL_DIR ${VES_DEV_DIR}/VTK-5.8/install-64-bit
#setenv XERCES_INSTALL_DIR="/home/users/mccdo/dev/deps/xerces-c-3.1.1/install-64-bit"
#setenv ZLIB_INSTALL_DIR="/home/users/mccdo/dev/deps/zlib-1.2.5/install-64-bit"

setenv LD_LIBRARY_PATH ${JUGGLER_INSTALL_DIR}/lib64:${VTK_INSTALL_DIR}/lib/vtk-5.8
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${ACETAO_INSTALL_DIR}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/usr/lib64
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${OSG_INSTALL_DIR}/lib:${OSG_INSTALL_DIR}/lib/osgPlugins
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BDFX_INSTALL_DIR}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BOOST_INSTALL_DIR}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BULLET_INSTALL_DIR}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${CPPDOM_INSTALL_DIR}/lib64
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${OSGBULLET_INSTALL_DIR}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${OSGBULLETPLUS_INSTALL_DIR}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${OSGEPHEMERIS_INSTALL_DIR}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${OSGWORKS_INSTALL_DIR}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${POCO_INSTALL_DIR}/lib
#setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${PNG_INSTALL_DIR}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/home/users/mccdo/TSVEG/ve-suite/trunk/install-rhel5.1_64/lib64
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/home/users/mccdo/VE_Suite_Deps/qt4.7.3-install/lib

setenv PATH ${JUGGLER_INSTALL_DIR}/bin:${ACETAO_INSTALL_DIR}/bin:${OSG_INSTALL_DIR}/bin:${JDK_HOME}/bin:${PATH}
setenv PATH ${PATH}:/home/users/mccdo/TSVEG/ve-suite/trunk/install-rhel5.1_64/bin

setenv FLAGPOLL_PATH ${GMTL_INSTALL_DIR}/share/flagpoll

setenv OSG_FILE_PATH ${BDFX_INSTALL_DIR}/share/backdropFX/data
setenv OSG_FILE_PATH ${OSGBULLET_INSTALL_DIR}/share/osgBullet/data:${OSG_FILE_PATH}
setenv OSG_FILE_PATH ${OSGBULLETPLUS_INSTALL_DIR}/share/osgBulletPlus/data:${OSG_FILE_PATH}
setenv OSG_FILE_PATH ${OSGWORKS_INSTALL_DIR}/share/osgWorks/data:${OSG_FILE_PATH}

#setenv DZR_BASE_DIR   ${VJ_BASE_DIR}/share/Doozer
#setenv PATH ${VJ_BASE_DIR}/bin:${PATH}
#setenv PATH ${WX_HOME}/bin:${PATH}
setenv PATH /home/users/mccdo/VE_Suite_Deps/qt4.7.3-install/bin:${PATH}
#setenv OSG_FILE_PATH /home/users/mccdo/svn_VE_Suite/VE_Suite/install_osg2.0_vrj2.2_rhel4_32/share/vesuite
#setenv PATH ${JDK_HOME}/bin:${PATH}
