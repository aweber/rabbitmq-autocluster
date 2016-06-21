#!/bin/sh

set -e

# add debug
[[ -n "$DEBUG" ]] && set -x

# allow the container to be started with `--user`
if [ "$1" = 'rabbitmq-server' -a "$(id -u)" = '0' ]; then
	chown -R rabbitmq:rabbitmq ${HOME}
	exec su-exec rabbitmq "$0" "$@"
fi

exec "$@"
