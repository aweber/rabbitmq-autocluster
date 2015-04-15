#!/bin/bash
CONSUL_VERSION=${CONSUL_VERSION:-0.5.0}
if [ ! -d "${HOME}/consul" ]; then
  echo "Downloading Consul ${CONSUL_VERSION}";
  curl -s -L -o /tmp/${CONSUL_VERSION}_linux_amd64.zip https://dl.bintray.com/mitchellh/consul/${CONSUL_VERSION}_linux_amd64.zip
  mkdir $HOME/consul
  cd $HOME/consul
  unzip /tmp/${CONSUL_VERSION}_linux_amd64.zip
fi
