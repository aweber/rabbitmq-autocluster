#!/usr/bin/env bash
set -euo pipefail
set -x

ROOT=$(readlink -f $(dirname $0)/../..)

cat $ROOT/packaging/deb/Dockerfile | docker build -t autocluster-deb-builder -

cd $ROOT

docker run -e 'HOME=/tmp/' -e 'GPG_PASSPHRASE=some_passphrase' --rm -i -t -v $(pwd):$(pwd) -u $(id -u) -w $(pwd) autocluster-deb-builder make package-deb VERSION=0.7.1-pre1
