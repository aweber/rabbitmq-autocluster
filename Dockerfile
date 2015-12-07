FROM gavinmroy/alpine-rabbitmq
MAINTAINER gavinr@aweber.com
ADD autocluster-0.0.0.ez /usr/lib/rabbitmq/plugins/
RUN chown -R rabbitmq /usr/lib/rabbitmq/plugins && \
    chmod -R a+r /usr/lib/rabbitmq/plugins/ && \
    rabbitmq-plugins enable --offline autocluster
