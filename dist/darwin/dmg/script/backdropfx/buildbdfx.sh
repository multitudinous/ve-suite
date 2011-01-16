#!/bin/sh

# --- Set build aliases --- #
alias buildbdfx="cd ${BACKDROPFX_SRC_DIR}; \
                 svn up; \
                 cd ${BACKDROPFX_BUILD_DIR}; \
                 cmake ${BACKDROPFX_SRC_DIR} \
                   -DCMAKE_BUILD_TYPE=\"RelWithDebInfo\" \
                   -DCMAKE_INSTALL_PREFIX=\"${BACKDROPFX_INSTALL_DIR}\" \
                   -DBDFX_BUILD_APPS:BOOL=OFF \
                   -DBDFX_BUILD_EXAMPLES:BOOL=ON \
                   -DBDFX_BUILD_PROTOS:BOOL=ON \
                   -DBDFX_BUILD_TESTS:BOOL=ON \
                   -DBDFX_PROFILE_ENABLE:BOOL=OFF \
                   -DBulletInstallType=\"Alternate Install Location\" \
                   -DOSGInstallType=\"Alternate Install Location\" \
                   -DBulletInstallLocation:PATH=\"${BULLET_INSTALL_DIR}\" \
                   -DOSGInstallLocation:PATH=\"${OSG_INSTALL_DIR}\"
                   -DOSGWORKS_INCLUDE_DIR=\"${OSGWORKS_INSTALL_DIR}/include\" \
                   -DOSGBULLET_ROOT=\"${OSGBULLET_INSTALL_DIR}\" \
                   -DOSGBULLETPLUS_ROOT=\"${OSGBULLETPLUS_INSTALL_DIR}\" \
                   -DOSGEPHEMERIS_ROOT=\"${OSGEPHEMERIS_INSTALL_DIR}\" \
                   -DBoost_INCLUDE_DIR:PATH=\"${BOOST_INSTALL_DIR}/include\"; \
                 make -j2 install;"

