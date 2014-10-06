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

# Add the root .ssh files
RUN mkdir -m 0700 -p /root/.ssh
ADD ssh/id_rsa /root/.ssh/id_rsa
ADD ssh/id_rsa.pub /root/.ssh/id_rsa.pub
ADD ssh/ssh_config /root/.ssh/config
RUN \
  chmod 0700 /root/.ssh/id_rsa && \
  cp /root/.ssh/id_rsa.pub /root/.ssh/authorized_keys && \
  mkdir /var/run/sshd && \
  rm -f /etc/cron.daily/standard


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
RUN apt-get -qq update && apt-get install -y -qq curl build-essential git rabbitmq-server ssh supervisor unzip && apt-get clean

# Install consul
RUN \
  curl -o /tmp/consul.zip -L https://dl.bintray.com/mitchellh/consul/0.4.0_linux_amd64.zip && \
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

ADD etc/supervisor/supervisord.conf /etc/supervisor/supervisord.conf
ADD etc/supervisor/conf.d/consul.conf /etc/supervisor/conf.d/consul.conf
ADD etc/supervisor/conf.d/rabbitmq.conf /etc/supervisor/conf.d/rabbitmq.conf
ADD etc/supervisor/conf.d/sshd.conf /etc/supervisor/conf.d/sshd.conf

ADD etc/rabbitmq/rabbitmq.config /etc/rabbitmq/
ADD var/lib/rabbitmq/.erlang.cookie /var/lib/rabbitmq/
ADD var/lib/rabbitmq/.erlang.cookie /root/
ADD rabbitmq_autocluster_consul-0.0.0.ez /usr/lib/rabbitmq/lib/rabbitmq_server-3.3.5/plugins/
RUN \
  chown rabbitmq:rabbitmq /var/lib/rabbitmq/.erlang.cookie && \
  rabbitmq-plugins enable rabbitmq_management rabbitmq_autocluster_consul

# For consul
ENV GOMAXPROCS 10

# Export the volumes for configuration and log file examination
VOLUME ["/etc/rabbitmq", "/var/log/rabbitmq", "/var/log/supervisor"]

# Set the HOME env variable to the right location for RabbitMQ to run
ENV HOME /var/lib/rabbitmq

# Ports to expose
EXPOSE 5672 15672 25672

# Seems to be dropped off
CMD ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisor/supervisord.conf"]
