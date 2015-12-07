FROM gavinmroy/alpine-rabbitmq
MAINTAINER gavinr@aweber.com
ADD autocluster-0.0.0.ez /usr/lib/rabbitmq/plugins/
RUN rabbitmq-plugins enable --offline rabbitmq_management autocluster
