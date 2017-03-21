RabbitMQ-Autoclsuter Consul example
--
Dynamic RabbitMQ cluster using:

1. [Docker compose](https://docs.docker.com/compose/)

2. [Consul](https://www.consul.io) 

3. [HA proxy](https://github.com/docker/dockercloud-haproxy)


Execute
--
```
git clone https://github.com/rabbitmq/rabbitmq-autocluster.git .
make dist
docker build -t rabbitmq/rabbitmq-autocluster .
cd  examples/compose_consul_haproxy 
docker-compose up
docker-compose scale rabbit=3
```

Check
--

Consul Management: http://localhost:8500/ui/ 
RabbitMQ Management: http://localhost:15672/#/
