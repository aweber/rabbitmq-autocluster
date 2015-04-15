#
# Base docker image for Ubuntu that sets up
#
FROM ubuntu:trusty
MAINTAINER gavinr@aweber.com

# Let aptitude know it's a non-interactive install
ENV DEBIAN_FRONTEND noninteractive
ENV HOME /root

# Hack for initctl
# See: https://github.com/dotcloud/docker/issues/1024
RUN \
  rm /sbin/initctl && \
  ln -sf /bin/true /sbin/initctl && \
  dpkg-divert --local --rename --add /sbin/initctl

# Don't let upstart start installed services
ADD usr/sbin/policy-rc.d /usr/sbin/policy-rc.d
RUN /bin/chmod 755 /usr/sbin/policy-rc.d

# Remove auto-installed cron jobs
RUN rm /etc/cron.daily/apt \
       /etc/cron.daily/dpkg \
       /etc/cron.daily/passwd \
       /etc/cron.daily/upstart \
       /etc/cron.weekly/fstrim

# Add RabbitMQ and Erlang repos
RUN \
  apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv F7B8CEA6056E8E56 && \
  echo "deb http://www.rabbitmq.com/debian/ testing main" > /etc/apt/sources.list.d/rabbitmq.list && \
  apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv D208507CA14F4FCA && \
  echo "deb http://packages.erlang-solutions.com/debian precise contrib" > /etc/apt/sources.list.d/erlang-solutions.list

# Update the repository indexes and install Erlang and RabbitMQ
RUN apt-get -qq update && apt-get install -y -qq curl build-essential git rabbitmq-server supervisor unzip && apt-get clean

# Install consul
RUN \
  curl -o /tmp/consul.zip -L https://dl.bintray.com/mitchellh/consul/0.5.0_linux_amd64.zip && \
  unzip /tmp/consul.zip -d /tmp/ && \
  chmod +x /tmp/consul && \
  mv /tmp/consul /usr/local/bin/consul && \
  rm /tmp/consul.zip && \
  mkdir -p /etc/consul.d

# Configuration files
ADD etc/security/limits.conf /etc/security/limits.conf
ADD etc/sysctl.conf /etc/sysctl.conf

ADD etc/consul.d/consul.json /etc/consul.d/consul.json
ADD etc/consul.d/amqp.json /etc/consul.d/amqp.json
ADD etc/consul.d/http.json /etc/consul.d/http.json

ADD etc/supervisord.conf /etc/
ADD etc/supervisor.d/consul.conf /etc/supervisor.d/
ADD etc/supervisor.d/rabbitmq.conf /etc/supervisor.d/

ADD etc/rabbitmq/rabbitmq.config /etc/rabbitmq/
ADD var/lib/rabbitmq/.erlang.cookie /var/lib/rabbitmq/
ADD var/lib/rabbitmq/.erlang.cookie /root/
ADD rabbitmq_autocluster_consul-0.2.0.ez /usr/lib/rabbitmq/lib/rabbitmq_server-3.5.1/plugins/
RUN \
  chown rabbitmq:rabbitmq /var/lib/rabbitmq/.erlang.cookie && \
  chmod 0600 /var/lib/rabbitmq/.erlang.cookie && \
  chmod 0600 /root/.erlang.cookie && \
  rabbitmq-plugins enable --offline rabbitmq_management rabbitmq_autocluster_consul

# For consul
ENV GOMAXPROCS 10

# Export the volumes for configuration and log file examination
VOLUME ["/etc/rabbitmq", "/var/log/rabbitmq", "/var/log/supervisor"]

# Set the HOME env variable to the right location for RabbitMQ to run
ENV HOME /var/lib/rabbitmq

EXPOSE 5672 15672 25672

ENTRYPOINT ["/usr/bin/supervisord", "-n"]
