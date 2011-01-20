#!/bin/sh

# --- Set dev & deps paths --- #
export DEV_BASE_DIR=${HOME}/dev
export DEPS_BASE_DIR=${DEV_BASE_DIR}/deps

# --- Set ace+tao paths --- #
export ACETAO_BASE_DIR=${DEPS_BASE_DIR}/ace+tao
export ACETAO_SRC_DIR=${ACETAO_BASE_DIR}/ace+tao_5.7.9
export ACETAO_BUILD_DIR=${ACETAO_BASE_DIR}/ace+tao_5.7.9_build
export ACETAO_INSTALL_DIR=${ACETAO_BASE_DIR}/ace+tao_5.7.9_install
export PATH=${ACETAO_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${ACETAO_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}
export PKG_CONFIG_PATH=${ACETAO_INSTALL_DIR}/lib/pkgconfig:${PKG_CONFIG_PATH}

# --- Set backdropfx paths --- #
export BACKDROPFX_BASE_DIR=${DEPS_BASE_DIR}/backdropfx
export BACKDROPFX_SRC_DIR=${BACKDROPFX_BASE_DIR}/backdropfx_trunk
export BACKDROPFX_BUILD_DIR=${BACKDROPFX_BASE_DIR}/backdropfx_trunk_build
export BACKDROPFX_INSTALL_DIR=${BACKDROPFX_BASE_DIR}/backdropfx_trunk_install
export PATH=${BACKDROPFX_INSTALL_DIR}/bin:${PATH}
export PATH=${BACKDROPFX_BUILD_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${BACKDROPFX_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}
export FLAGPOLL_PATH=${BACKDROPFX_INSTALL_DIR}/lib/flagpoll:${FLAGPOLL_PATH}
export OSG_FILE_PATH=${BACKDROPFX_SRC_DIR}/data:${OSG_FILE_PATH}

# --- Set boost paths --- #
export BOOST_BASE_DIR=${DEPS_BASE_DIR}/boost
export BOOST_SRC_DIR=${BOOST_BASE_DIR}/boost_1.45.0
export BOOST_INSTALL_DIR=${BOOST_BASE_DIR}/boost_1.45.0_install
export DYLD_LIBRARY_PATH=${BOOST_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}

# --- Set bullet paths --- #
export BULLET_BASE_DIR=${DEPS_BASE_DIR}/bullet
export BULLET_SRC_DIR=${BULLET_BASE_DIR}/bullet_2.77
export BULLET_BUILD_DIR=${BULLET_BASE_DIR}/bullet_2.77_build
export BULLET_INSTALL_DIR=${BULLET_BASE_DIR}/bullet_2.77_install
export DYLD_LIBRARY_PATH=${BULLET_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}
export PKG_CONFIG_PATH=${BULLET_INSTALL_DIR}/lib/pkgconfig:${PKG_CONFIG_PATH}

# --- Set cppdom paths --- #
export CPPDOM_BASE_DIR=${DEPS_BASE_DIR}/cppdom
export CPPDOM_SRC_DIR=${CPPDOM_BASE_DIR}/cppdom_1.0.3
export CPPDOM_INSTALL_DIR=${CPPDOM_BASE_DIR}/cppdom_1.0.3_install
export PATH=${CPPDOM_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${CPPDOM_INSTALL_DIR}/lib64:${DYLD_LIBRARY_PATH}
export FLAGPOLL_PATH=${CPPDOM_INSTALL_DIR}/lib64/flagpoll:${FLAGPOLL_PATH}

# --- Set ctags paths --- #
export CTAGS_BASE_DIR=${DEPS_BASE_DIR}/ctags
export CTAGS_SRC_DIR=${CTAGS_BASE_DIR}/ctags_5.8
export CTAGS_BUILD_DIR=${CTAGS_BASE_DIR}/ctags_5.8_build
export CTAGS_INSTALL_DIR=${CTAGS_BASE_DIR}/ctags_5.8_install
export PATH=${CTAGS_INSTALL_DIR}/bin:${PATH}
# --- Set path where tags will be installed --- #
export TAGS_DIR=${HOME}/.vim/tags

# --- Set flagpoll paths --- #
export FLAGPOLL_BASE_DIR=${DEPS_BASE_DIR}/flagpoll
export FLAGPOLL_SRC_DIR=${FLAGPOLL_BASE_DIR}/flagpoll_0.9.4
export FLAGPOLL_INSTALL_DIR=${FLAGPOLL_BASE_DIR}/flagpoll_0.9.4_install
export PATH=${FLAGPOLL_INSTALL_DIR}/bin:${PATH}
export FLAGPOLL_PATH=${FLAGPOLL_INSTALL_DIR}/share/flagpoll:${FLAGPOLL_PATH}
export ACLOCAL_FLAGS="-I ${FLAGPOLL_INSTALL_DIR}/share/aclocal ${ACLOCAL_FLAGS}"

# --- Set gmtl paths --- #
export GMTL_BASE_DIR=${DEPS_BASE_DIR}/gmtl
export GMTL_SRC_DIR=${GMTL_BASE_DIR}/gmtl_0.6.1
export GMTL_INSTALL_DIR=${GMTL_BASE_DIR}/gmtl_0.6.1_install
export PATH=${GMTL_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${GMTL_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}
export FLAGPOLL_PATH=${GMTL_INSTALL_DIR}/share/flagpoll:${FLAGPOLL_PATH}

# --- Set juggler paths --- #
export JUGGLER_BASE_DIR=${DEPS_BASE_DIR}/juggler
export JUGGLER_SRC_DIR=${JUGGLER_BASE_DIR}/juggler_trunk
export JUGGLER_BUILD_DIR=${JUGGLER_BASE_DIR}/juggler_trunk_build
export JUGGLER_INSTALL_DIR=${JUGGLER_BASE_DIR}/juggler_trunk_install
export PATH=${JUGGLER_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${JUGGLER_INSTALL_DIR}/lib/x86_64/debug:${DYLD_LIBRARY_PATH}
export DYLD_LIBRARY_PATH=${JUGGLER_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}
export FLAGPOLL_PATH=${JUGGLER_INSTALL_DIR}/lib/flagpoll:${FLAGPOLL_PATH}

# --- Set osg paths --- #
export OSG_BASE_DIR=${DEPS_BASE_DIR}/osg
export OSG_SRC_DIR=${OSG_BASE_DIR}/osg_2.8.3
export OSG_BUILD_DIR=${OSG_BASE_DIR}/osg_2.8.3_build
export OSG_INSTALL_DIR=${OSG_BASE_DIR}/osg_2.8.3_install
export PATH=${OSG_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${OSG_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}
export DYLD_LIBRARY_PATH=${OSG_INSTALL_DIR}/lib/osgPlugins-2.8.3:${DYLD_LIBRARY_PATH}
export OSG_FILE_PATH=${OSG_BASE_DIR}/osg-data:${OSG_FILE_PATH}
export OSG_DIR=${OSG_INSTALL_DIR} #For osgBullet build

# --- Set osgaudio paths --- #
export OSGAUDIO_BASE_DIR=${DEPS_BASE_DIR}/osgaudio
export OSGAUDIO_SRC_DIR=${OSGAUDIO_BASE_DIR}/osgaudio_trunk
export OSGAUDIO_BUILD_DIR=${OSGAUDIO_BASE_DIR}/osgaudio_trunk_build
export OSGAUDIO_INSTALL_DIR=${OSGAUDIO_BASE_DIR}/osgaudio_trunk_install

# --- Set osgbullet paths --- #
export OSGBULLET_BASE_DIR=${DEPS_BASE_DIR}/osgbullet
export OSGBULLET_SRC_DIR=${OSGBULLET_BASE_DIR}/osgbullet_trunk
export OSGBULLET_BUILD_DIR=${OSGBULLET_BASE_DIR}/osgbullet_trunk_build
export OSGBULLET_INSTALL_DIR=${OSGBULLET_BASE_DIR}/osgbullet_trunk_install
export PATH=${OSGBULLET_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${OSGBULLET_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}
export FLAGPOLL_PATH=${OSGBULLET_INSTALL_DIR}/lib/flagpoll:${FLAGPOLL_PATH}
export OSG_FILE_PATH=${OSGBULLET_SRC_DIR}/data:${OSG_FILE_PATH}

# --- Set osgbulletplus paths --- #
export OSGBULLETPLUS_BASE_DIR=${DEPS_BASE_DIR}/osgbulletplus
export OSGBULLETPLUS_SRC_DIR=${OSGBULLETPLUS_BASE_DIR}/osgbulletplus_trunk
export OSGBULLETPLUS_BUILD_DIR=${OSGBULLETPLUS_BASE_DIR}/osgbulletplus_trunk_build
export OSGBULLETPLUS_INSTALL_DIR=${OSGBULLETPLUS_BASE_DIR}/osgbulletplus_trunk_install
export PATH=${OSGBULLETPLUS_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${OSGBULLETPLUS_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}
export FLAGPOLL_PATH=${OSGBULLETPLUS_INSTALL_DIR}/lib/flagpoll:${FLAGPOLL_PATH}

# --- Set osgephemeris paths --- #
export OSGEPHEMERIS_BASE_DIR=${DEPS_BASE_DIR}/osgephemeris
export OSGEPHEMERIS_SRC_DIR=${OSGEPHEMERIS_BASE_DIR}/osgephemeris_trunk
export OSGEPHEMERIS_BUILD_DIR=${OSGEPHEMERIS_BASE_DIR}/osgephemeris_trunk_build
export OSGEPHEMERIS_INSTALL_DIR=${OSGEPHEMERIS_BASE_DIR}/osgephemeris_trunk_install
export DYLD_LIBRARY_PATH=${OSGEPHEMERIS_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}
export FLAGPOLL_PATH=${OSGEPHEMERIS_INSTALL_DIR}/lib/flagpoll:${FLAGPOLL_PATH}

# --- Set osgworks paths --- #
export OSGWORKS_BASE_DIR=${DEPS_BASE_DIR}/osgworks
export OSGWORKS_SRC_DIR=${OSGWORKS_BASE_DIR}/osgworks_trunk
export OSGWORKS_BUILD_DIR=${OSGWORKS_BASE_DIR}/osgworks_trunk_build
export OSGWORKS_INSTALL_DIR=${OSGWORKS_BASE_DIR}/osgworks_trunk_install
export PATH=${OSGWORKS_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${OSGWORKS_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}
export FLAGPOLL_PATH=${OSGWORKS_INSTALL_DIR}:${FLAGPOLL_PATH}
export OSG_FILE_PATH=${OSGWORKS_SRC_DIR}/data:${OSG_FILE_PATH}

 #--- Set performer paths --- #
#export PERFORMER_BASE_DIR=${DEPS_BASE_DIR}/performer
#export PERFORMER_SRC_DIR=${PERFORMER_BASE_DIR}/performer_3.2.4
#export PERFORMER_BUILD_DIR=${PERFORMER_BASE_DIR}/performer_3.2.4_build
#export PERFORMER_INSTALL_DIR=${PERFORMER_BASE_DIR}/performer_3.2.4_install
#export PATH=${PERFORMER_INSTALL_DIR}/X11R6/bin:${PATH}
#export DYLD_LIBRARY_PATH=${PERFORMER_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}
#export LM_LICENSE_FILE=${PERFORMER_INSTALL_DIR}/share/Performer/license.dat

# --- Set poco paths --- #
export POCO_BASE_DIR=${DEPS_BASE_DIR}/poco
export POCO_SRC_DIR=${POCO_BASE_DIR}/poco_1.4.0
export POCO_BUILD_DIR=${POCO_BASE_DIR}/poco_1.4.0_build
export POCO_INSTALL_DIR=${POCO_BASE_DIR}/poco_1.4.0_install
export PATH=${POCO_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${POCO_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}

# --- Set scons paths --- #
export SCONS_BASE_DIR=${DEPS_BASE_DIR}/scons
export SCONS_SRC_DIR=${SCONS_BASE_DIR}/scons_2.0.1
export SCONS_INSTALL_DIR=${SCONS_BASE_DIR}/scons_2.0.1_install
export PATH=${SCONS_INSTALL_DIR}/bin:${PATH}

# --- Set ves paths --- #
export VES_BASE_DIR=${DEPS_BASE_DIR}/ves
export VES_SRC_DIR=${DEV_BASE_DIR}/ve-suite/trunk
export VES_BUILD_DIR=${VES_BASE_DIR}/ves_trunk_build
export VES_INSTALL_DIR=${VES_BASE_DIR}/ves_trunk_install
export PATH=${VES_SRC_DIR}:${PATH}
export PATH=${VES_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${VES_INSTALL_DIR}/lib64:${DYLD_LIBRARY_PATH}
export OSG_NOTIFY_LEVEL=WARN
export OSG_THREAD_SAFE_REF_UNREF=1
export TAO_MACHINE=localhost
export TAO_PORT=1239

# --- Set vtk paths --- #
export VTK_BASE_DIR=${DEPS_BASE_DIR}/vtk
export VTK_SRC_DIR=${VTK_BASE_DIR}/vtk_5.4.2
export VTK_BUILD_DIR=${VTK_BASE_DIR}/vtk_5.4.2_build
export VTK_INSTALL_DIR=${VTK_BASE_DIR}/vtk_5.4.2_install
export PATH=${VTK_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${VTK_INSTALL_DIR}/lib/vtk-5.4:${DYLD_LIBRARY_PATH}

# --- Set xerces paths --- #
export XERCES_BASE_DIR=${DEPS_BASE_DIR}/xerces
export XERCES_SRC_DIR=${XERCES_BASE_DIR}/xerces_3.1.1
export XERCES_BUILD_DIR=${XERCES_BASE_DIR}/xerces_3.1.1_build
export XERCES_INSTALL_DIR=${XERCES_BASE_DIR}/xerces_3.1.1_install
export PATH=${XERCES_INSTALL_DIR}/bin:${PATH}
export DYLD_LIBRARY_PATH=${XERCES_INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}

# --- Set build functions --- #
function usage()
{
  echo "
    usage: $0 options

    This function builds the named package.

    OPTIONS:
      -h      Show this message
      -c      Clean the build directory before building
      -u      Update the source code before building
      -j      Build with multithreading enabled
              Requires argument to specify number of jobs (1:8) to use
      -t      Create tag file with exuberant ctags" >&2
}

function argscase()
{
  declare -a args=( 0 0 1 0 )
  while getopts "hcutj:" opts
  do
    case $opts in
      h)
        usage
        kill -SIGINT $$
        ;;
      c)
        args[0]=1
        ;;
      u)
        args[1]=1
        ;;
      j)
        if [[ $OPTARG =~ [^1-8] ]] ; then
          echo "Error: '$OPTARG' not a valid number." >&2;
          usage;
          kill -SIGINT $$;
        fi
        args[2]=$OPTARG
        ;;
      t)
        args[3]=1
        ;;
      ?)
        echo "Invalid option: $OPTARG" >&2
        usage
        kill -SIGINT $$
        ;;
      :)
        echo "Option $OPTARG requires an argument." >&2
        usage
        kill -SIGINT $$
        ;;
    esac
  done
  shift $(($OPTIND - 1))
  echo "${args[@]}"
}

function ctags()
{
  ${CTAGS_INSTALL_DIR}/bin/ctags -RI --c++-kinds=+p --fields=+iaS --extra=+q --languages=c++ .
  rm ${TAGS_DIR}/${1}
  mv tags ${TAGS_DIR}/${1}
}

function bld_acetao()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${ACETAO_BUILD_DIR}/*; fi
  cd ${ACETAO_BUILD_DIR}
  ${ACETAO_SRC_DIR}/configure \
    --disable-tao-tests \
    --disable-tao-examples \
    --disable-ace-tests \
    --disable-ace-examples \
    --prefix=${ACETAO_INSTALL_DIR}
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${ACETAO_INSTALL_DIR}/include; ctags acetao; fi
}

function bld_bdfx()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${BACKDROPFX_BUILD_DIR}/*; fi
  if [ ${args[1]} == 1 ]; then cd ${BACKDROPFX_SRC_DIR}; svn up; fi
  cd ${BACKDROPFX_BUILD_DIR}
  cmake ${BACKDROPFX_SRC_DIR} \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=${BACKDROPFX_INSTALL_DIR} \
    -DBDFX_BUILD_APPS=OFF \
    -DBDFX_BUILD_EXAMPLES=ON \
    -DBDFX_BUILD_PROTOS=ON \
    -DBDFX_BUILD_TESTS=ON \
    -DBDFX_PROFILE_ENABLE=OFF \
    -DBulletInstallType="Alternate Install Location" \
    -DOSGInstallType="Alternate Install Location" \
    -DBulletInstallLocation=${BULLET_INSTALL_DIR} \
    -DOSGInstallLocation=${OSG_INSTALL_DIR} \
    -DOSGWORKS_INCLUDE_DIR=${OSGWORKS_INSTALL_DIR}/include \
    -DOSGBULLET_ROOT=${OSGBULLET_INSTALL_DIR} \
    -DOSGBULLETPLUS_ROOT=${OSGBULLETPLUS_INSTALL_DIR} \
    -DOSGEPHEMERIS_ROOT=${OSGEPHEMERIS_INSTALL_DIR} \
    -DBoost_INCLUDE_DIR=${BOOST_INSTALL_DIR}/include
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${BACKDROPFX_INSTALL_DIR}/include; ctags bdfx; fi
}

function bld_boost()
{
  declare -a args=( `argscase "$@"` )
  cd ${BOOST_SRC_DIR}
  bash bootstrap.sh \
    --prefix=${BOOST_INSTALL_DIR}
  ./bjam -j${args[2]} \
    variant=release \
    link=shared \
    threading=multi \
    install
  if [ ${args[3]} == 1 ]; then cd ${BOOST_INSTALL_DIR}/include; ctags boost; fi
}

function bld_bullet()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${BULLET_BUILD_DIR}/*; fi
  cd ${BULLET_BUILD_DIR}
  cmake ${BULLET_SRC_DIR} \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=${BULLET_INSTALL_DIR} \
    -DINCLUDE_INSTALL_DIR=${BULLET_INSTALL_DIR}/include \
    -DLIB_DESTINATION=${BULLET_INSTALL_DIR}/lib \
    -DPKGCONFIG_INSTALL_PREFIX=${BULLET_INSTALL_DIR}/pkgconfig \
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
    -DUSE_MSVC_RUNTIME_LIBRARY_DLL=OFF
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${BULLET_INSTALL_DIR}/include; ctags bullet; fi
}

function bld_cppdom()
{
  declare -a args=( `argscase "$@"` )
  cd ${CPPDOM_SRC_DIR}
  scons install -j${args[2]} \
    var_arch=x64 \
    var_type=optimized \
    var_libtype=shared \
    darwin_sdk=/Developer/SDKs/MacOSX10.6.sdk \
    prefix=${CPPDOM_INSTALL_DIR}
  if [ ${args[3]} == 1 ]; then cd ${CPPDOM_INSTALL_DIR}/include; ctags cppdom; fi
}

function bld_flagpoll()
{
  declare -a args=( `argscase "$@"` )
  cd ${FLAGPOLL_SRC_DIR}
  python setup.py install \
    --prefix=${FLAGPOLL_INSTALL_DIR}
}

function bld_gmtl()
{
  declare -a args=( `argscase "$@"` )
  cd ${GMTL_SRC_DIR}
  scons install -j${args[2]} \
    prefix=${GMTL_INSTALL_DIR}
  if [ ${args[3]} == 1 ]; then cd ${GMTL_INSTALL_DIR}/include; ctags gmtl; fi
}

function bld_juggler()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${JUGGLER_BUILD_DIR}/*; fi
  if [ ${args[1]} == 1 ]; then cd ${JUGGLER_SRC_DIR}; svn up; fi
  bash autogen.sh
  cd ${JUGGLER_BUILD_DIR}
  ${JUGGLER_SRC_DIR}/configure.pl \
    --with-boost=${BOOST_INSTALL_DIR} \
    --with-boost-includes=${BOOST_INSTALL_DIR}/include \
    --prefix=${JUGGLER_INSTALL_DIR}
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${JUGGLER_INSTALL_DIR}/include; ctags juggler; fi
}

function bld_osg()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${OSG_BUILD_DIR}/*; fi
  if [ ${args[1]} == 1 ]; then cd ${OSG_SRC_DIR}; svn up; fi
  cd ${OSG_BUILD_DIR}
  cmake ${OSG_SRC_DIR} \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=${OSG_INSTALL_DIR} \
    -DCMAKE_OSX_ARCHITECTURES=x86_64 \
    -DOSG_WINDOWING_SYSTEM=Cocoa
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${OSG_INSTALL_DIR}/include; ctags osg; fi
}

function bld_osgaudio()
{
  declare -a args=( `argscase "$@"` )
  #if [ ${args[3]} == 1 ]; then cd ${OSGAUDIO_INSTALL_DIR}/include; ctags osgaudio; fi
}

function bld_osgbullet()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${OSGBULLET_BUILD_DIR}/*; fi
  if [ ${args[1]} == 1 ]; then cd ${OSGBULLET_SRC_DIR}; svn up; fi
  cd ${OSGBULLET_BUILD_DIR}
  cmake ${OSGBULLET_SRC_DIR} \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=${OSGBULLET_INSTALL_DIR} \
    -DBUILD_SHARED_LIBS=ON \
    -DOSGBULLET_BUILD_APPLICATIONS=ON \
    -DOSGBULLET_BUILD_EXAMPLES=OFF \
    -DOSGBULLET_BUILD_TESTS=OFF \
    -DOSGBULLET_USE_DOUBLE_PRECISION=OFF \
    -DBulletInstallType="Alternate Install Location" \
    -DOSGInstallType="Alternate Install Location" \
    -DBulletInstallLocation=${BULLET_INSTALL_DIR} \
    -DOSGInstallLocation=${OSG_INSTALL_DIR} \
    -DOSGWORKS_INCLUDE_DIR=${OSGWORKS_INSTALL_DIR}/include
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${OSGBULLET_INSTALL_DIR}/include; ctags osgbullet; fi
}

function bld_osgbulletplus()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${OSGBULLETPLUS_BUILD_DIR}/*; fi
  if [ ${args[1]} == 1 ]; then cd ${OSGBULLETPLUS_SRC_DIR}; svn up; fi
  cd ${OSGBULLETPLUS_BUILD_DIR}
  cmake ${OSGBULLETPLUS_SRC_DIR} \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=${OSGBULLETPLUS_INSTALL_DIR} \
    -DOSGBULLETPLUS_BUILD_APPLICATIONS=ON \
    -DOSGBULLETPLUS_BUILD_EXAMPLES=OFF \
    -DOSGBULLETPLUS_BUILD_PROTOS=OFF \
    -DOSGBULLETPLUS_BUILD_TESTS=OFF \
    -DOSGBULLETPLUS_USE_DOUBLE_PRECISION=OFF \
    -DOSGBULLET_ROOT=${OSGBULLET_INSTALL_DIR} \
    -DBulletInstallType="Alternate Install Location" \
    -DOSGInstallType="Alternate Install Location" \
    -DBulletInstallLocation=${BULLET_INSTALL_DIR} \
    -DOSGInstallLocation=${OSG_INSTALL_DIR} \
    -DOSGWORKS_INCLUDE_DIR=${OSGWORKS_INSTALL_DIR}/include
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${OSGBULLETPLUS_INSTALL_DIR}/include; ctags osgbulletplus; fi
}

function bld_osgephemeris()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${OSGEPHEMERIS_BUILD_DIR}/*; fi
  if [ ${args[1]} == 1 ]; then cd ${OSGEPHEMERIS_SRC_DIR}; svn up; fi
  cd ${OSGEPHEMERIS_BUILD_DIR}
  cmake ${OSGEPHEMERIS_SRC_DIR} \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=${OSGEPHEMERIS_INSTALL_DIR}
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${OSGEPHEMERIS_INSTALL_DIR}/include; ctags osgephemeris; fi
}

function bld_osgworks()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${OSGWORKS_BUILD_DIR}/*; fi
  if [ ${args[1]} == 1 ]; then cd ${OSGWORKS_SRC_DIR}; svn up; fi
  cd ${OSGWORKS_BUILD_DIR}
  cmake ${OSGWORKS_SRC_DIR} \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=${OSGWORKS_INSTALL_DIR} \
    -DCMAKE_OSX_ARCHITECTURES=x86_64 \
    -DOSGInstallType="Alternate Install Location" \
    -DOSGInstallLocation=${OSG_INSTALL_DIR} \
    -DBoost_INCLUDE_DIR=${BOOST_INSTALL_DIR}/include
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${OSGWORKS_INSTALL_DIR}/include; ctags osgworks; fi
}

function bld_poco()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${POCO_BUILD_DIR}/*; fi
  cd ${POCO_BUILD_DIR}
  ${POCO_SRC_DIR}/configure \
    --no-tests \
    --no-samples \
    --omit=Data/MySQL \
    --config=Darwin64 \
    --prefix=${POCO_INSTALL_DIR}
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${POCO_INSTALL_DIR}/include; ctags poco; fi
}

function bld_scons()
{
  declare -a args=( `argscase "$@"` )
  cd ${SCONS_SRC_DIR}
  python setup.py install \
    --prefix=${SCONS_INSTALL_DIR}
}

function bld_ves()
{
  declare -a args=( `argscase "$@"` )
  #if [ ${args[3]} == 1 ]; then cd ${VES_INSTALL_DIR}/include; ctags ves; fi
}

function bld_vtk()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${VTK_BUILD_DIR}/*; fi
  cd ${VTK_BUILD_DIR}
  cmake ${VTK_SRC_DIR} \
    -DCMAKE_BUILD_TYPE=RelWithDebInfo \
    -DCMAKE_INSTALL_PREFIX=${VTK_INSTALL_DIR} \
    -DBUILD_SHARED_LIBS=ON \
    -DBUILD_TESTING=OFF \
    -DVTK_USE_PARALLEL=ON
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${VTK_INSTALL_DIR}/include; ctags vtk; fi
}

function bld_xerces()
{
  declare -a args=( `argscase "$@"` )
  if [ ${args[0]} == 1 ]; then rm -rf ${XERCES_BUILD_DIR}/*; fi
  cd ${XERCES_BUILD_DIR}
  ${XERCES_SRC_DIR}/configure \
    CFLAGS="-arch x86_64" \
    CXXFLAGS="-arch x86_64" \
    --prefix=${XERCES_INSTALL_DIR}
  make install -j${args[2]}
  if [ ${args[3]} == 1 ]; then cd ${XERCES_INSTALL_DIR}/include; ctags xerces; fi
}

