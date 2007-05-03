#!/bin/csh
# sets up environment to build and/or run VE-Suite

#export VPR_DEBUG_ALLOW_CATEGORIES="VES_DBG"
#export VPR_DEBUG_DISALLOW_CATEGORIES ="VES_DBG DBG_KERNEL"
export OSG_THREAD_SAFE_REF_UNREF=1

export FLAGPOLL_PATH=${VE_SUITE_HOME}/VE_Installer/fpc:/Volumes/data/VE_Suite_Deps/vrjuggler-2.0-svn/build-darwin/instlinks/lib/flagpoll
export FLAGPOLL_PATH=${FLAGPOLL_PATH}:/Volumes/data/VE_Suite_Deps/vrjuggler/cppdom-0.6.6/lib/flagpoll
export FLAGPOLL_PATH=${FLAGPOLL_PATH}:/Volumes/data/VE_Suite_Deps/vrjuggler/gmtl-0.4.12-install/share/pkgconfig
export FLAGPOLL_PATH=${FLAGPOLL_PATH}:/Volumes/data/VE_Suite_Deps/ACE_TAO/install/lib/pkgconfig:/opt/local/lib/pkgconfig

#setenv JDK_HOME /usr/java
export VTK_BASE_DIR=
#`flagpoll vtk --get-prefix`
export TAO_HOME=`flagpoll TAO --get-prefix`
#export WX_HOME=/Volumes/data/VE_Suite_Deps/wxWidgets/install
export VJ_BASE_DIR=`flagpoll vrjuggler --get-prefix`
export OSG_HOME=
#`flagpoll osg --get-prefix`
#setenv CORONA_HOME /home/vr/Applications/TSVEG/Libraries/Release/Opt/corona-1.0.2/Linux-SuSE92

export DYLD_LIBRARY_PATH=${VJ_BASE_DIR}/lib:${VTK_BASE_DIR}/lib
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ImageIO.framework/Versions/A/Resources
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${OSG_HOME}/lib:${OSG_HOME}/lib/osgPlugins
#export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${WX_HOME}/lib
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${VE_SUITE_HOME}/test_two/lib
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${TAO_HOME}/lib
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:/Volumes/data/VE_Suite_Deps/opal-install/lib
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:/opt/local/lib

export PATH=${TAO_HOME}/bin:${PATH}

export PATH=${VJ_BASE_DIR}/bin:${VE_SUITE_HOME}/test_two/bin:${PATH}
export PATH=${OSG_HOME}/share/OpenSceneGraph/bin:${PATH}
export OSG_FILE_PATH=${OSG_HOME}/share/OpenSceneGraph-Data
