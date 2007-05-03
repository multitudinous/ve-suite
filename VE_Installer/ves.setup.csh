#!/bin/csh
# sets up environment to build and/or run VE-Suite

#setenv VPR_DEBUG_ALLOW_CATEGORIES "VES_DBG"
#setenv VPR_DEBUG_DISALLOW_CATEGORIES "VES_DBG DBG_KERNEL"
setenv OSG_THREAD_SAFE_REF_UNREF 1
   
# These are used to build VE-Suite
setenv FLAGPOLL_PATH /home/vr/Applications/TSVEG/Libraries/Release/Opt/VRJuggler-2.0.1-branch/Linux-RHEL4/lib/flagpoll
setenv FLAGPOLL_PATH ${FLAGPOLL_PATH}:/home/vr/Applications/TSVEG/Libraries/Release/Opt/cppdom-0.6.6/Linux-RHEL4/lib/flagpoll
setenv FLAGPOLL_PATH ${FLAGPOLL_PATH}:/home/vr/Applications/TSVEG/Libraries/Release/Opt/GMTL-0.4.12/Linux-RHEL4/share/pkgconfig
setenv FLAGPOLL_PATH ${FLAGPOLL_PATH}:/home/vr/Applications/TSVEG/Libraries/Release/Opt/ACE-TAO-5.5/Linux-RHEL_4/lib/pkgconfig

# These are used to run VE-Suite
#setenv JDK_HOME /usr/java
setenv VTK_BASE_DIR /home/vr/Applications/TSVEG/Libraries/Release/Opt/VTK-5.0/Linux-RHEL4
#`flagpoll vtk --get-prefix`
setenv TAO_HOME "`flagpoll TAO --get-prefix`"
setenv WX_HOME /home/vr/Applications/TSVEG/Libraries/Release/Opt/wxGTK-2.8.0/Linux-RHEL4_32
setenv VJ_BASE_DIR "`flagpoll vrjuggler --get-prefix`"
setenv OSG_HOME /home/vr/Applications/TSVEG/Libraries/Release/Opt/OSG-1.2/Linux-RHEL_4
#`flagpoll osg --get-prefix`
#setenv CORONA_HOME /home/vr/Applications/TSVEG/Libraries/Release/Opt/corona-1.0.2/Linux-SuSE92

setenv  LD_LIBRARY_PATH ${VJ_BASE_DIR}/lib:${VTK_BASE_DIR}/lib
setenv  LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${OSG_HOME}/lib:${OSG_HOME}/lib/osgPlugins
setenv  LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${WX_HOME}/lib
setenv  LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${TAO_HOME}/lib
#setenv  LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/Volumes/data/VE_Suite_Deps/opal-install/lib
#setenv  LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/opt/local/lib
setenv  LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/home/users/mccdo/svn_VE_Suite/VE_Suite/test_two/lib

setenv OSG_FILE_PATH ${OSG_HOME}/share/OpenSceneGraph-Data
setenv PATH ${TAO_HOME}/bin:${PATH}
setenv PATH ${OSG_HOME}/share/OpenSceneGraph/bin:${PATH}
setenv PATH ${VJ_BASE_DIR}/bin:/home/users/mccdo/svn_VE_Suite/VE_Suite/test_two/bin:${PATH}
