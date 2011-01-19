REM !/bin/sh
REM tortoiseproc.exe /command:update /path:. /closeonend:1
REM http://www.dostips.com/DtTutoFunctions.php
REM http://tortoisesvn.net/docs/release/TortoiseSVN_en/tsvn-automation.html

REM  --- Set vim config path --- #
REM set VIM_CONFIG_DIR=%HOME%/.vim
set HOME=C:
REM --- Set dev & deps paths --- #
set DEV_BASE_DIR=%HOME%/dev
set DEPS_BASE_DIR=%DEV_BASE_DIR%/deps

REM --- Set ace+tao paths --- #
set ACETAO_BASE_DIR=%DEPS_BASE_DIR%/ace+tao
set ACETAO_SRC_DIR=%ACETAO_BASE_DIR%/ace+tao_5.7.9
set ACETAO_BUILD_DIR=%ACETAO_BASE_DIR%/ace+tao_5.7.9_build
set ACETAO_INSTALL_DIR=%ACETAO_BASE_DIR%/ace+tao_5.7.9_install
set PATH=%ACETAO_INSTALL_DIR%/bin:%PATH%
set PATH=%ACETAO_INSTALL_DIR%/lib:%PATH%
set PKG_CONFIG_PATH=%ACETAO_INSTALL_DIR%/lib/pkgconfig:%PKG_CONFIG_PATH%

REM --- Set backdropfx paths --- #
set BACKDROPFX_BASE_DIR=%DEPS_BASE_DIR%/backdropfx
set BACKDROPFX_SRC_DIR=%BACKDROPFX_BASE_DIR%/backdropfx_trunk
set BACKDROPFX_BUILD_DIR=%BACKDROPFX_BASE_DIR%/backdropfx_trunk_build
set BACKDROPFX_INSTALL_DIR=%BACKDROPFX_BASE_DIR%/backdropfx_trunk_install
set PATH=%BACKDROPFX_INSTALL_DIR%/bin:%PATH%
set PATH=%BACKDROPFX_BUILD_DIR%/bin:%PATH%
set PATH=%BACKDROPFX_INSTALL_DIR%/lib:%PATH%
set FLAGPOLL_PATH=%BACKDROPFX_INSTALL_DIR%/lib/flagpoll:%FLAGPOLL_PATH%
set OSG_FILE_PATH=%BACKDROPFX_SRC_DIR%/data:%OSG_FILE_PATH%

REM --- Set boost paths --- #
set BOOST_BASE_DIR=%DEPS_BASE_DIR%/boost
set BOOST_SRC_DIR=%BOOST_BASE_DIR%/boost_1.45.0
set BOOST_INSTALL_DIR=%BOOST_BASE_DIR%/boost_1.45.0_install
set PATH=%BOOST_INSTALL_DIR%/lib:%PATH%

REM --- Set bullet paths --- #
set BULLET_BASE_DIR=%DEPS_BASE_DIR%/bullet
set BULLET_SRC_DIR=%BULLET_BASE_DIR%/bullet_2.77
set BULLET_BUILD_DIR=%BULLET_BASE_DIR%/bullet_2.77_build
set BULLET_INSTALL_DIR=%BULLET_BASE_DIR%/bullet_2.77_install
set PATH=%BULLET_INSTALL_DIR%/lib:%PATH%
set PKG_CONFIG_PATH=%BULLET_INSTALL_DIR%/lib/pkgconfig:%PKG_CONFIG_PATH%

REM --- Set cppdom paths --- #
set CPPDOM_BASE_DIR=%DEPS_BASE_DIR%/cppdom
set CPPDOM_SRC_DIR=%CPPDOM_BASE_DIR%/cppdom_1.0.3
set CPPDOM_INSTALL_DIR=%CPPDOM_BASE_DIR%/cppdom_1.0.3_install
set PATH=%CPPDOM_INSTALL_DIR%/bin:%PATH%
set PATH=%CPPDOM_INSTALL_DIR%/lib64:%PATH%
set FLAGPOLL_PATH=%CPPDOM_INSTALL_DIR%/lib64/flagpoll:%FLAGPOLL_PATH%

REM --- Set ctags paths --- #
set CTAGS_BASE_DIR=%DEPS_BASE_DIR%/ctags
set CTAGS_SRC_DIR=%CTAGS_BASE_DIR%/ctags_5.8
set CTAGS_BUILD_DIR=%CTAGS_BASE_DIR%/ctags_5.8_build
set CTAGS_INSTALL_DIR=%CTAGS_BASE_DIR%/ctags_5.8_install
set PATH=%CTAGS_INSTALL_DIR%/bin:%PATH%
ctagscmd="ctags -RI --c++-kinds=+p --fields=+iaS --extra=+q --languages=c++ ."

REM --- Set flagpoll paths --- #
set FLAGPOLL_BASE_DIR=%DEPS_BASE_DIR%/flagpoll
set FLAGPOLL_SRC_DIR=%FLAGPOLL_BASE_DIR%/flagpoll_0.9.4
set FLAGPOLL_INSTALL_DIR=%FLAGPOLL_BASE_DIR%/flagpoll_0.9.4_install
set PATH=%FLAGPOLL_INSTALL_DIR%/bin:%PATH%
set FLAGPOLL_PATH=%FLAGPOLL_INSTALL_DIR%/share/flagpoll:%FLAGPOLL_PATH%
set ACLOCAL_FLAGS="-I %FLAGPOLL_INSTALL_DIR%/share/aclocal %ACLOCAL_FLAGS%"

REM --- Set gmtl paths --- #
set GMTL_BASE_DIR=%DEPS_BASE_DIR%/gmtl
set GMTL_SRC_DIR=%GMTL_BASE_DIR%/gmtl_0.6.1
set GMTL_INSTALL_DIR=%GMTL_BASE_DIR%/gmtl_0.6.1_install
set PATH=%GMTL_INSTALL_DIR%/bin:%PATH%
set PATH=%GMTL_INSTALL_DIR%/lib:%PATH%
set FLAGPOLL_PATH=%GMTL_INSTALL_DIR%/share/flagpoll:%FLAGPOLL_PATH%

REM --- Set juggler paths --- #
set JUGGLER_BASE_DIR=%DEPS_BASE_DIR%/juggler
set JUGGLER_SRC_DIR=%JUGGLER_BASE_DIR%/juggler_trunk
set JUGGLER_BUILD_DIR=%JUGGLER_BASE_DIR%/juggler_trunk_build
set JUGGLER_INSTALL_DIR=%JUGGLER_BASE_DIR%/juggler_trunk_install
set PATH=%JUGGLER_INSTALL_DIR%/bin:%PATH%
set PATH=%JUGGLER_INSTALL_DIR%/lib/x86_64/debug:%PATH%
set PATH=%JUGGLER_INSTALL_DIR%/lib:%PATH%
set FLAGPOLL_PATH=%JUGGLER_INSTALL_DIR%/lib/flagpoll:%FLAGPOLL_PATH%

REM --- Set osg paths --- #
set OSG_BASE_DIR=%DEPS_BASE_DIR%/osg
set OSG_SRC_DIR=%OSG_BASE_DIR%/osg_2.8.3
set OSG_BUILD_DIR=%OSG_BASE_DIR%/osg_2.8.3_build
set OSG_INSTALL_DIR=%OSG_BASE_DIR%/osg_2.8.3_install
set PATH=%OSG_INSTALL_DIR%/bin:%PATH%
set PATH=%OSG_INSTALL_DIR%/lib:%PATH%
set PATH=%OSG_INSTALL_DIR%/lib/osgPlugins-2.8.3:%PATH%
set OSG_FILE_PATH=%OSG_BASE_DIR%/osg-data:%OSG_FILE_PATH%
set OSG_DIR=%OSG_INSTALL_DIR% #For osgBullet build

REM --- Set osgaudio paths --- #
set OSGAUDIO_BASE_DIR=%DEPS_BASE_DIR%/osgaudio
set OSGAUDIO_SRC_DIR=%OSGAUDIO_BASE_DIR%/osgaudio_trunk
set OSGAUDIO_BUILD_DIR=%OSGAUDIO_BASE_DIR%/osgaudio_trunk_build
set OSGAUDIO_INSTALL_DIR=%OSGAUDIO_BASE_DIR%/osgaudio_trunk_install

REM --- Set osgbullet paths --- #
set OSGBULLET_BASE_DIR=%DEPS_BASE_DIR%/osgbullet
set OSGBULLET_SRC_DIR=%OSGBULLET_BASE_DIR%/osgbullet_trunk
set OSGBULLET_BUILD_DIR=%OSGBULLET_BASE_DIR%/osgbullet_trunk_build
set OSGBULLET_INSTALL_DIR=%OSGBULLET_BASE_DIR%/osgbullet_trunk_install
set PATH=%OSGBULLET_INSTALL_DIR%/bin:%PATH%
set PATH=%OSGBULLET_INSTALL_DIR%/lib:%PATH%
set FLAGPOLL_PATH=%OSGBULLET_INSTALL_DIR%/lib/flagpoll:%FLAGPOLL_PATH%
set OSG_FILE_PATH=%OSGBULLET_SRC_DIR%/data:%OSG_FILE_PATH%

REM --- Set osgbulletplus paths --- #
set OSGBULLETPLUS_BASE_DIR=%DEPS_BASE_DIR%/osgbulletplus
set OSGBULLETPLUS_SRC_DIR=%OSGBULLETPLUS_BASE_DIR%/osgbulletplus_trunk
set OSGBULLETPLUS_BUILD_DIR=%OSGBULLETPLUS_BASE_DIR%/osgbulletplus_trunk_build
set OSGBULLETPLUS_INSTALL_DIR=%OSGBULLETPLUS_BASE_DIR%/osgbulletplus_trunk_install
set PATH=%OSGBULLETPLUS_INSTALL_DIR%/bin:%PATH%
set PATH=%OSGBULLETPLUS_INSTALL_DIR%/lib:%PATH%
set FLAGPOLL_PATH=%OSGBULLETPLUS_INSTALL_DIR%/lib/flagpoll:%FLAGPOLL_PATH%

REM --- Set osgephemeris paths --- #
set OSGEPHEMERIS_BASE_DIR=%DEPS_BASE_DIR%/osgephemeris
set OSGEPHEMERIS_SRC_DIR=%OSGEPHEMERIS_BASE_DIR%/osgephemeris_trunk
set OSGEPHEMERIS_BUILD_DIR=%OSGEPHEMERIS_BASE_DIR%/osgephemeris_trunk_build
set OSGEPHEMERIS_INSTALL_DIR=%OSGEPHEMERIS_BASE_DIR%/osgephemeris_trunk_install
set PATH=%OSGEPHEMERIS_INSTALL_DIR%/lib:%PATH%
set FLAGPOLL_PATH=%OSGEPHEMERIS_INSTALL_DIR%/lib/flagpoll:%FLAGPOLL_PATH%

REM --- Set osgworks paths --- #
set OSGWORKS_BASE_DIR=%DEPS_BASE_DIR%/osgworks
set OSGWORKS_SRC_DIR=%OSGWORKS_BASE_DIR%/osgworks_trunk
set OSGWORKS_BUILD_DIR=%OSGWORKS_BASE_DIR%/osgworks_trunk_build
set OSGWORKS_INSTALL_DIR=%OSGWORKS_BASE_DIR%/osgworks_trunk_install
set PATH=%OSGWORKS_INSTALL_DIR%/bin:%PATH%
set PATH=%OSGWORKS_INSTALL_DIR%/lib:%PATH%
set FLAGPOLL_PATH=%OSGWORKS_INSTALL_DIR%:%FLAGPOLL_PATH%
set OSG_FILE_PATH=%OSGWORKS_SRC_DIR%/data:%OSG_FILE_PATH%

 #--- Set performer paths --- #
#set PERFORMER_BASE_DIR=%DEPS_BASE_DIR%/performer
#set PERFORMER_SRC_DIR=%PERFORMER_BASE_DIR%/performer_3.2.4
#set PERFORMER_BUILD_DIR=%PERFORMER_BASE_DIR%/performer_3.2.4_build
#set PERFORMER_INSTALL_DIR=%PERFORMER_BASE_DIR%/performer_3.2.4_install
#set PATH=%PERFORMER_INSTALL_DIR%/X11R6/bin:%PATH%
#set PATH=%PERFORMER_INSTALL_DIR%/lib:%PATH%
#set LM_LICENSE_FILE=%PERFORMER_INSTALL_DIR%/share/Performer/license.dat

REM --- Set poco paths --- #
set POCO_BASE_DIR=%DEPS_BASE_DIR%/poco
set POCO_SRC_DIR=%POCO_BASE_DIR%/poco_1.4.0
set POCO_BUILD_DIR=%POCO_BASE_DIR%/poco_1.4.0_build
set POCO_INSTALL_DIR=%POCO_BASE_DIR%/poco_1.4.0_install
set PATH=%POCO_INSTALL_DIR%/bin:%PATH%
set PATH=%POCO_INSTALL_DIR%/lib:%PATH%

REM --- Set scons paths --- #
set SCONS_BASE_DIR=%DEPS_BASE_DIR%/scons
set SCONS_SRC_DIR=%SCONS_BASE_DIR%/scons_2.0.1
set SCONS_INSTALL_DIR=%SCONS_BASE_DIR%/scons_2.0.1_install
set PATH=%SCONS_INSTALL_DIR%/bin:%PATH%

REM --- Set ves paths --- #
set VES_BASE_DIR=%DEPS_BASE_DIR%/ves
set VES_SRC_DIR=%DEV_BASE_DIR%/ve-suite/trunk
set VES_BUILD_DIR=%VES_BASE_DIR%/ves_trunk_build
set VES_INSTALL_DIR=%VES_BASE_DIR%/ves_trunk_install
set PATH=%VES_SRC_DIR%:%PATH%
set PATH=%VES_INSTALL_DIR%/bin:%PATH%
set PATH=%VES_INSTALL_DIR%/lib64:%PATH%
set OSG_NOTIFY_LEVEL=WARN
set OSG_THREAD_SAFE_REF_UNREF=1
set TAO_MACHINE=localhost
set TAO_PORT=1239

REM --- Set vtk paths --- #
set VTK_BASE_DIR=%DEPS_BASE_DIR%/vtk
set VTK_SRC_DIR=%VTK_BASE_DIR%/vtk_5.4.2
set VTK_BUILD_DIR=%VTK_BASE_DIR%/vtk_5.4.2_build
set VTK_INSTALL_DIR=%VTK_BASE_DIR%/vtk_5.4.2_install
set PATH=%VTK_INSTALL_DIR%/bin:%PATH%
set PATH=%VTK_INSTALL_DIR%/lib/vtk-5.4:%PATH%

REM --- Set xerces paths --- #
set XERCES_BASE_DIR=%DEPS_BASE_DIR%/xerces
set XERCES_SRC_DIR=%XERCES_BASE_DIR%/xerces_3.1.1
set XERCES_BUILD_DIR=%XERCES_BASE_DIR%/xerces_3.1.1_build
set XERCES_INSTALL_DIR=%XERCES_BASE_DIR%/xerces_3.1.1_install
set PATH=%XERCES_INSTALL_DIR%/bin:%PATH%
set PATH=%XERCES_INSTALL_DIR%/lib:%PATH%

REM --- Set build aliases --- #
alias buildacetao=" \
  cd %ACETAO_BUILD_DIR%; \
  %ACETAO_SRC_DIR%/configure \
    --disable-tao-tests \
    --disable-tao-examples \
    --disable-ace-tests \
    --disable-ace-examples \
    --prefix=%ACETAO_INSTALL_DIR%; \
  make install; \
  cd %ACETAO_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/acetao; \
  mv tags %VIM_CONFIG_DIR%/tags/acetao;"

:buildBDFX
  cd %BACKDROPFX_SRC_DIR%
  tortoiseproc.exe /command:update /path:. /closeonend:1
  cd %BACKDROPFX_BUILD_DIR%
  cmake %BACKDROPFX_SRC_DIR% ^
    -DCMAKE_BUILD_TYPE=RelWithDebInfo ^
    -DCMAKE_INSTALL_PREFIX=%BACKDROPFX_INSTALL_DIR% ^
    -DBDFX_BUILD_APPS=OFF ^
    -DBDFX_BUILD_EXAMPLES=ON ^
    -DBDFX_BUILD_PROTOS=ON ^
    -DBDFX_BUILD_TESTS=ON ^
    -DBDFX_PROFILE_ENABLE=OFF ^
    -DBulletInstallType="Alternate Install Location" ^
    -DOSGInstallType="Alternate Install Location" ^
    -DBulletInstallLocation=%BULLET_INSTALL_DIR% ^
    -DOSGInstallLocation=%OSG_INSTALL_DIR% ^
    -DOSGWORKS_INCLUDE_DIR=%OSGWORKS_INSTALL_DIR%/include ^
    -DOSGBULLET_ROOT=%OSGBULLET_INSTALL_DIR% ^
    -DOSGBULLETPLUS_ROOT=%OSGBULLETPLUS_INSTALL_DIR% ^
    -DOSGEPHEMERIS_ROOT=%OSGEPHEMERIS_INSTALL_DIR% ^
    -DBoost_INCLUDE_DIR=%BOOST_INSTALL_DIR%/include ^
    -G "NMake Makefiles" 
  nmake install
REM  cd %BACKDROPFX_INSTALL_DIR%/include
REM   $ctagscmd
REM   rm %VIM_CONFIG_DIR%/tags/bdfx; \
REM   mv tags %VIM_CONFIG_DIR%/tags/bdfx;"
goto:eof

alias buildboost=" \
  cd %BOOST_SRC_DIR%; \
  bash bootstrap.sh \
    --prefix=%BOOST_INSTALL_DIR%; \
  ./bjam \
    variant=release \
    link=shared \
    threading=multi \
    install; \
  cd %BOOST_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/boost; \
  mv tags %VIM_CONFIG_DIR%/tags/boost;"

alias buildbullet=" \
  cd %BULLET_BUILD_DIR%; \
  cmake %BULLET_SRC_DIR% \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=%BULLET_INSTALL_DIR% \
    -DINCLUDE_INSTALL_DIR=%BULLET_INSTALL_DIR%/include \
    -DLIB_DESTINATION=%BULLET_INSTALL_DIR%/lib \
    -DPKGCONFIG_INSTALL_PREFIX=%BULLET_INSTALL_DIR%/pkgconfig \
    -DBUILD_CPU_DEMOS=OFF \
    -DBUILD_DEMOS=OFF \
    -DBUILD_EXTRAS=ON \
    -DBUILD_MINICL_OPENCL_DEMOS=OFF \
    -DBUILD_UNIT_TESTS=OFF \
    -DINSTALL_EXTRA_LIBS=ON \
    -DINSTALL_LIBS=ON \
    -DUSE_CUSTOM_VECTOR_MATH=OFF \
    -DUSE_DOUBLE_PRECISION=OFF \
    -DUSE_GLUT=OFF \
    -DUSE_GRAPHICAL_BENCHMARK=OFF \
    -DUSE_MSVC_RUNTIME_LIBRARY_DLL=OFF \
    -G "NMake Makefiles"; \
  make install; \
  cd %BULLET_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/bullet; \
  mv tags %VIM_CONFIG_DIR%/tags/bullet;"

alias buildcppdom=" \
  cd %CPPDOM_SRC_DIR%; \
  scons install \
    var_arch=x64 \
    var_type=optimized \
    var_libtype=shared \
    darwin_sdk=/Developer/SDKs/MacOSX10.6.sdk \
    prefix=%CPPDOM_INSTALL_DIR%; \
  cd %CPPDOM_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/cppdom; \
  mv tags %VIM_CONFIG_DIR%/tags/cppdom;"

alias buildflagpoll=" \
  cd %FLAGPOLL_SRC_DIR%; \
  python setup.py install \
    --prefix=%FLAGPOLL_INSTALL_DIR%;"

alias buildgmtl=" \
  cd %GMTL_SRC_DIR%; \
  scons install \
    prefix=%GMTL_INSTALL_DIR%;
  cd %GMTL_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/gmtl; \
  mv tags %VIM_CONFIG_DIR%/tags/gmtl;"

alias buildjuggler=" \
  cd %JUGGLER_SRC_DIR%; \
  svn up; \
  bash autogen.sh; \
  cd %JUGGLER_BUILD_DIR%; \
  %JUGGLER_SRC_DIR%/configure.pl \
    --with-boost=%BOOST_INSTALL_DIR% \
    --with-boost-includes=%BOOST_INSTALL_DIR%/include \
    --prefix=%JUGGLER_INSTALL_DIR%; \
  make install; \
  cd %JUGGLER_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/juggler; \
  mv tags %VIM_CONFIG_DIR%/tags/juggler;"

alias buildosg=" \
  cd %OSG_BUILD_DIR%; \
  cmake %OSG_SRC_DIR% \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=%OSG_INSTALL_DIR% \
    -DCMAKE_OSX_ARCHITECTURES=x86_64 \
    -DOSG_WINDOWING_SYSTEM=Cocoa \
    -G "NMake Makefiles" \
  make install; \
  cd %OSG_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/osg; \
  mv tags %VIM_CONFIG_DIR%/tags/osg;"

alias buildosgaudio=" \
"

alias buildosgbullet=" \
  cd %OSGBULLET_SRC_DIR%; \
  svn up; \
  cd %OSGBULLET_BUILD_DIR%; \
  cmake %OSGBULLET_SRC_DIR% \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=%OSGBULLET_INSTALL_DIR% \
    -DBUILD_SHARED_LIBS=ON \
    -DOSGBULLET_BUILD_APPLICATIONS=ON \
    -DOSGBULLET_BUILD_EXAMPLES=OFF \
    -DOSGBULLET_BUILD_TESTS=OFF \
    -DOSGBULLET_USE_DOUBLE_PRECISION=OFF \
    -DBulletInstallType=\"Alternate Install Location\" \
    -DOSGInstallType=\"Alternate Install Location\" \
    -DBulletInstallLocation=%BULLET_INSTALL_DIR% \
    -DOSGInstallLocation=%OSG_INSTALL_DIR% \
    -DOSGWORKS_INCLUDE_DIR=%OSGWORKS_INSTALL_DIR%/include \
    -G "NMake Makefiles"; \
  make install; \
  cd %OSGBULLET_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/osgbullet; \
  mv tags %VIM_CONFIG_DIR%/tags/osgbullet;"

alias buildosgbulletplus=" \
  cd %OSGBULLETPLUS_SRC_DIR%; \
  svn up; \
  cd %OSGBULLETPLUS_BUILD_DIR%; \
  cmake %OSGBULLETPLUS_SRC_DIR% \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=%OSGBULLETPLUS_INSTALL_DIR% \
    -DOSGBULLETPLUS_BUILD_APPLICATIONS=ON \
    -DOSGBULLETPLUS_BUILD_EXAMPLES=OFF \
    -DOSGBULLETPLUS_BUILD_PROTOS=OFF \
    -DOSGBULLETPLUS_BUILD_TESTS=OFF \
    -DOSGBULLETPLUS_USE_DOUBLE_PRECISION=OFF \
    -DOSGBULLET_ROOT=%OSGBULLET_INSTALL_DIR% \
    -DBulletInstallType=\"Alternate Install Location\" \
    -DOSGInstallType=\"Alternate Install Location\" \
    -DBulletInstallLocation=%BULLET_INSTALL_DIR% \
    -DOSGInstallLocation=%OSG_INSTALL_DIR% \
    -DOSGWORKS_INCLUDE_DIR=%OSGWORKS_INSTALL_DIR%/include \
    -G "NMake Makefiles"; \
  make install; \
  cd %OSGBULLETPLUS_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/osgbulletplus; \
  mv tags %VIM_CONFIG_DIR%/tags/osgbulletplus;"

alias buildosgephemeris=" \
  cd %OSGEPHEMERIS_SRC_DIR%; \
  svn up; \
  cd %OSGEPHEMERIS_BUILD_DIR%; \
  cmake %OSGEPHEMERIS_SRC_DIR% \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=%OSGEPHEMERIS_INSTALL_DIR% \
    -G "NMake Makefiles"; \
  make install; \
  cd %OSGEPHEMERIS_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/osgephemeris; \
  mv tags %VIM_CONFIG_DIR%/tags/osgephemeris;"

alias buildosgworks=" \
  cd %OSGWORKS_SRC_DIR%; \
  svn up; \
  cd %OSGWORKS_BUILD_DIR%; \
  cmake %OSGWORKS_SRC_DIR% \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=%OSGWORKS_INSTALL_DIR% \
    -DCMAKE_OSX_ARCHITECTURES=x86_64 \
    -DOSGInstallType=\"Alternate Install Location\" \
    -DOSGInstallLocation=%OSG_INSTALL_DIR% \
    -DBoost_INCLUDE_DIR=%BOOST_INSTALL_DIR%/include \
    -G "NMake Makefiles"; \
  make install; \
  cd %OSGWORKS_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/osgworks; \
  mv tags %VIM_CONFIG_DIR%/tags/osgworks;"

alias buildpoco=" \
  cd %POCO_BUILD_DIR%; \
  %POCO_SRC_DIR%/configure \
    --no-tests \
    --no-samples \
    --omit=Data/MySQL \
    --config=Darwin64 \
    --prefix=%POCO_INSTALL_DIR%; \
  make install; \
  cd %POCO_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/poco; \
  mv tags %VIM_CONFIG_DIR%/tags/poco;"

alias buildscons=" \
  cd %SCONS_SRC_DIR%; \
  python setup.py install \
    --prefix=%SCONS_INSTALL_DIR%;"

alias buildvtk=" \
  cd %VTK_BUILD_DIR%; \
  cmake %VTK_SRC_DIR% \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=%VTK_INSTALL_DIR% \
    -DBUILD_SHARED_LIBS=ON \
    -DBUILD_TESTING=OFF \
    -DVTK_USE_PARALLEL=ON \
    -G "NMake Makefiles"; \
  make install; \
  cd %VTK_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/vtk; \
  mv tags %VIM_CONFIG_DIR%/tags/vtk;"

alias buildxerces=" \
  cd %XERCES_BUILD_DIR%; \
  %XERCES_SRC_DIR%/configure \
    CFLAGS=\"-arch x86_64\" \
    CXXFLAGS=\"-arch x86_64\" \
    --prefix=%XERCES_INSTALL_DIR%; \
  make install; \
  cd %XERCES_INSTALL_DIR%/include; \
  $ctagscmd; \
  rm %VIM_CONFIG_DIR%/tags/xerces; \
  mv tags %VIM_CONFIG_DIR%/tags/xerces;"

