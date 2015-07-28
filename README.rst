rabbitmq-autocluster-consul
===========================
An *experimental* RabbitMQ plugin that clusters nodes automatically using Consul.

.. image:: https://img.shields.io/travis/aweber/rabbitmq-autocluster-consul.svg
    :target: https://travis-ci.org/aweber/rabbitmq-autocluster-consul
.. image:: https://img.shields.io/github/release/aweber/rabbitmq-autocluster-consul.svg
    :target: https://github.com/aweber/rabbitmq-autocluster-consul/releases

Download
--------
Downloads of rabbitmq-autocluster-consul can be found on the
`GitHub Releases <https://github.com/aweber/rabbitmq-autocluster-consul/releases>`_ page.
Check for version compatibility in the release notes.

Installation
------------
Place the  RabbitMQ plugins directory. Once
extracted, run ``rabbitmq-plugins enable rabbitmq_autocluster_consul``.

Configuration
-------------

Configuration for the plugin can be set in two places: operating system environment variables
or the ``rabbitmq.config`` file.


You can change the settings for the plugin in the ``rabbitmq.config`` file under
a ``rabbitmq_autocluster_consul`` stanza. The configuration can change any of these
default values:

Settings
````````
**Consul URL Scheme**

*The URI scheme to use when connecting to Consul*

+----------------------+------------------------+
| Environment Variable | ``CONSUL_SCHEME``      |
+----------------------+------------------------+
| Setting Key          | ``consul_scheme``      |
+----------------------+------------------------+
| Data type            | ``list``               |
+----------------------+------------------------+
| Default Value        | ``http``               |
+----------------------+------------------------+

**Consul Host**

*The hostname to use when connecting to Consul's API*

+----------------------+------------------------+
| Environment Variable | ``CONSUL_HOST``        |
+----------------------+------------------------+
| Setting Key          | ``consul_host``        |
+----------------------+------------------------+
| Data type            | ``list``               |
+----------------------+------------------------+
| Default Value        | ``localhost``          |
+----------------------+------------------------+

**Consul Port**

*The port to use when connecting to Consul's API*

+----------------------+------------------------+
| Environment Variable | ``CONSUL_PORT``        |
+----------------------+------------------------+
| Setting Key          | ``consul_port``        |
+----------------------+------------------------+
| Data type            | ``int``                |
+----------------------+------------------------+
| Default Value        | ``8500``               |
+----------------------+------------------------+

**Consul ACL Token**

*The Consul access token to use when registering the node with Consul (optional)*

+----------------------+------------------------+
| Environment Variable | ``CONSUL_ACL``         |
+----------------------+------------------------+
| Setting Key          | ``consul_acl``         |
+----------------------+------------------------+
| Data type            | ``list``               |
+----------------------+------------------------+
| Default Value        | null                   |
+----------------------+------------------------+

**RabbitMQ Cluster Name**

*A RabbitMQ cluster name to restrict the cluster membership to (optional)*

+----------------------+-------------------------+
| Environment Variable | ``CLUSTER_NAME``        |
+----------------------+-------------------------+
| Setting Key          | ``cluster_name``        |
+----------------------+-------------------------+
| Data type            | ``list``                |
+----------------------+-------------------------+
| Default Value        | null                    |
+----------------------+-------------------------+

**Consul Service Name**

*The name of the service to register with Consul for automatic clustering*

+----------------------+-------------------------+
| Environment Variable | ``SERVICE_NAME``        |
+----------------------+-------------------------+
| Setting Key          | ``consul_service``      |
+----------------------+-------------------------+
| Data type            | ``list``                |
+----------------------+-------------------------+
| Default Value        | ``rabbitmq``            |
+----------------------+-------------------------+

**Consul Service Prefix**

*A prefix that is prepending to the service name when registering with Consul
for automatic clustering (optional)*

+----------------------+---------------------------+
| Environment Variable | ``SERVICE_PREFIX``        |
+----------------------+---------------------------+
| Setting Key          | ``consul_service_prefix`` |
+----------------------+---------------------------+
| Data type            | ``list``                  |
+----------------------+---------------------------+
| Default Value        | null                      |
+----------------------+---------------------------+

**Consul Service Port**

*Used to set a port for the service in Consul, allowing for the automatic clustering
service registration to double as a general RabbitMQ service registration*

+----------------------+-------------------------+
| Environment Variable | ``SERVICE_PORT``        |
+----------------------+-------------------------+
| Setting Key          | ``consul_service_port`` |
+----------------------+-------------------------+
| Data type            | ``list``                |
+----------------------+-------------------------+
| Default Value        | ``5672``                |
+----------------------+-------------------------+

*Exaple rabbitmq.config*

..  code-block:: erlang

    [{rabbitmq_autocluster_consul,
      [
        {consul_host, "localhost"},
        {consul_port, 8500},
        {consul_acl, "example-acl-token"},
        {consul_service, "rabbitmq-test"},
        {cluster_name, "test"}
      ]}
    ].

Building
--------
Steps to custom build a version of the ``rabbitmq-autocluster-consul`` plugin:

.. code-block:: bash

    git clone https://github.com/rabbitmq/rabbitmq-public-umbrella.git
    cd rabbitmq-public-umbrella
    make co
    make BRANCH=rabbitmq_v3_5_4 up_c
    git clone https://github.com/aweber/rabbitmq-autocluster-consul.git
    cd rabbitmq-autocluster-consul
    make

Test and Development Environment
--------------------------------
The included Vagrant based virtual environment includes everything required to
test and/or develop on the plugin. Upon creation of the CoreOS based virtual
machine, Consul will be downloaded and setup, running in the base operating system
as a server. Note that there is nothing docker specific about the project and it
can run anywhere you use Consul.

Once you have started the VM with ``vagrant up`` you should be able to connect to
the Consul UI at http://192.168.150.10:8500/ui/.

The container is Ubuntu based and includes latest stable Erlang and RabbitMQ
release installed. Pre-built containers for testing are available on Docker Hub under
the ``aweber/rabbitmq-autocluster-consul`` tag.

Starting the Testing Environment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can run the first container to get access to the management UI with:

.. code-block:: bash

    docker run -d -p 15672:15672 --dns 127.0.0.1 --dns-search node.rmq.consul aweber/rabbitmq-autocluster-consul

The RabbitMQ management UI will the be available at http://192.168.150.10:15672/ and
you can login with the username/password combo of ``guest``/``guest``.

You can run subsequent containers with:

.. code-block:: bash

    docker run -d --dns 127.0.0.1 --dns-search node.rmq.consul aweber/rabbitmq-autocluster-consul

Starting the Development Environment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
For creation of the docker image, you should first compile the plugin from the project directory
and copy the plugin's ez file from ``dist`` to the project docker directory:

.. code-block:: bash

    make
    cp dist/rabbitmq_autocluster_consul-0.0.0.ez docker

The build artifact will be copied into the Docker image that is created and enabled
so that it works out of the box. Once copied, build the local image:

.. code-block:: bash

    docker build -t rabbitmq-autocluster-consul /home/core/share/rabbitmq-autocluster-consul/docker/

Use the following docker command to start the first container:

.. code-block:: bash

    docker run -d -p 15672:15672 --dns 127.0.0.1 --dns-search node.rmq.consul \
      -v /home/core/share:/opt/rabbitmq-public-umbrella rabbitmq-autocluster-consul

And the following command for all subsequent containers:

.. code-block:: bash

    docker run -d --dns 127.0.0.1 --dns-search node.rmq.consul \
      -v /home/core/share:/opt/rabbitmq-public-umbrella rabbitmq-autocluster-consul

The ``rabbitmq-public-umbrella`` directory is available under ``/opt/source`` in the container.

Development Notes
^^^^^^^^^^^^^^^^^
- If you intend to do development against the containerized cluster, once you get your
  first container up and running you should use ``docker exec`` to connect into it and stop
  RabbitMQ with ``supervisorctl stop rabbitmq``. Then you'll want to run RabbitMQ out of the
  ``/opt/source/rabbitmq-public-umbrella/rabbitmq-server`` directory by running
  ``make run``. Then you can do the same in each container you want to work with.
- When you compile the source in the ``rabbitmq-autocluster-consul`` directory
  on your host machine, all of the containers will see the change in their
  ``/opt/source/rabbitmq-public-umbrella/rabbitmq-autocluster-consul`` directory.
