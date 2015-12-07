FROM gavinmroy/alpine-rabbitmq
MAINTAINER gavinr@aweber.com
ADD dist/autocluster-0.0.0.ez /usr/lib/rabbitmq/plugins/
RUN rabbitmq-plugins enable --offline autocluster
