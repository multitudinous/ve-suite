#!/bin/sh

# --- Set build aliases --- #
alias buildjuggler="cd ${JUGGLER_SRC_DIR}; \
                    svn up; \
                    bash autogen.sh; \
                    cd ${JUGGLER_BUILD_DIR}; \
                    ${JUGGLER_SRC_DIR}/configure.pl \
                      --with-boost=${BOOST_INSTALL_DIR} \
                      --with-boost-includes=${BOOST_INSTALL_DIR}/include \
                      --prefix=${JUGGLER_INSTALL_DIR}; \
                    make -j2 install;"

