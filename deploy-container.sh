#!/bin/bash
docker build -t aweber/rabbitmq-autocluster:$1 .
docker tag aweber/rabbitmq-autocluster:$1 aweber/rabbitmq-autocluster:latest
docker push aweber/rabbitmq-autocluster:$1
docker push aweber/rabbitmq-autocluster:latest
