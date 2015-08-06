#!/bin/bash
BUILD_DIR=${BUILD_DIR:-${HOME}/rabbitmq-public-umbrella}
RABBIT_TAG=${RABBIT_TAG:-v3_5_4}
echo "Build dir: ${BUILD_DIR}"
echo "Travis build dir: ${TRAVIS_BUILD_DIR}"
git clone https://github.com/rabbitmq/rabbitmq-public-umbrella.git ${BUILD_DIR}
cd ${BUILD_DIR}
git checkout -b rabbitmq_${RABBIT_TAG}
git pull origin rabbitmq_${RABBIT_TAG}
make co
if [ -d ${BUILD_DIR}/autocluster ]; then
    rm -rf ${BUILD_DIR}/autocluster
fi
mkdir -p ${BUILD_DIR}/autocluster
cp -r ${TRAVIS_BUILD_DIR}/* ${BUILD_DIR}/autocluster/
cd ${BUILD_DIR}/autocluster
