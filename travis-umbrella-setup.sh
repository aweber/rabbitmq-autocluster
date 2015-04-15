#!/bin/bash
BUILD_DIR=${BUILD_DIR:-${HOME}/rabbitmq-public-umbrella}
RABBIT_TAG=${RABBIT_TAG:-v3_5_1}
echo "Build dir: ${BUILD_DIR}"
echo "Travis build dir: ${TRAVIS_BUILD_DIR}"
if [ ! -d $BUILD_DIR ]; then
    git clone https://github.com/rabbitmq/rabbitmq-public-umbrella.git $BUILD_DIR
    cd $BUILD_DIR
    make co
fi
cd $BUILD_DIR
make BRANCH=rabbitmq_${RABBIT_TAG} up_c
rm -rf ${BUILD_DIR}/rabbitmq-autocluster-consul
cp -r ${TRAVIS_BUILD_DIR} ${BUILD_DIR}/
