include ../umbrella.mk

docker:
	docker build -t gavinmroy/alpine-rabbitmq-autocluster .
