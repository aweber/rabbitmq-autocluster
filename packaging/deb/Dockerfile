FROM ubuntu:precise
# FROM quay.io/travisci/travis-erlang
# FROM debian

RUN apt-get update

ENV DEBIAN_FRONTEND noninteractive
RUN apt-get -y install wget
RUN wget --no-check-certificate https://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_17.5-1~ubuntu~precise_amd64.deb -O /tmp/erlang.deb && dpkg -i /tmp/erlang.deb || apt-get -y -f install

RUN apt-get update \
 && apt-get install -y --no-install-recommends debhelper git rsync fakeroot build-essential python zip \
 && apt-get clean

# erlang-nox erlang-src erlang-dev