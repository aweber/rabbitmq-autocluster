RabbitMQ Autocluster
====================
An RabbitMQ plugin that clusters nodes automatically using `Consul <https://consul.io>`_,
`etcd2 <https://github.com/coreos/etcd>`_, or DNS for service discovery.

.. image:: https://img.shields.io/travis/aweber/rabbitmq-autocluster.svg
    :target: https://travis-ci.org/aweber/rabbitmq-autocluster
.. image:: https://img.shields.io/github/release/aweber/rabbitmq-autocluster.svg
    :target: https://github.com/aweber/rabbitmq-autocluster/releases

Download
--------
Downloads of autocluster can be found on the
`GitHub Releases <https://github.com/aweber/rabbitmq-autocluster/releases>`_ page.
Check for version compatibility in the release notes.

Installation
------------
Place the plugin in the RabbitMQ plugins directory. To enable, run ``rabbitmq-plugins enable autocluster``.

Configuration
-------------
Configuration for the plugin can be set in two places: operating system environment variables
or the ``rabbitmq.config`` file under the ``autocluster`` stanza.

General Settings
^^^^^^^^^^^^^^^^
The following settings apply to the general configuration of the plugin:

**Backend Type**

Which type of service discovery backend to use. One of ``consul``, ``etcd``, or ``dns``.

+----------------------+------------------------+
| Environment Variable | ``AUTOCLUSTER_TYPE``   |
+----------------------+------------------------+
| Setting Key          | ``backend``            |
+----------------------+------------------------+
| Data type            | ``atom``               |
+----------------------+------------------------+
| Default Value        | ``consul``             |
+----------------------+------------------------+

**Long name Support**

When set to ``true`` this will cause RabbitMQ and the autocluster plugin to use fully
qualified names to identify nodes.

+----------------------+---------------------------+
| Environment Variable | ``RABBITMQ_USE_LONGNAME`` |
+----------------------+---------------------------+
| Setting Key          | ``longname``              |
+----------------------+---------------------------+
| Data type            | ``bool``                  |
+----------------------+---------------------------+
| Default Value        | ``false``                 |
+----------------------+---------------------------+

**RabbitMQ Cluster Name**

A RabbitMQ cluster name to restrict the cluster membership to (optional). This only
works with the Consul and etcd backends*

+----------------------+-------------------------+
| Environment Variable | ``CLUSTER_NAME``        |
+----------------------+-------------------------+
| Setting Key          | ``cluster_name``        |
+----------------------+-------------------------+
| Data type            | ``list``                |
+----------------------+-------------------------+
| Default Value        | null                    |
+----------------------+-------------------------+

Consul Settings
^^^^^^^^^^^^^^^
The following settings apply to the consul backend only.

**Consul URL Scheme**

The URI scheme to use when connecting to Consul

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

The hostname to use when connecting to Consul's API

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

The port to use when connecting to Consul's API

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

The Consul access token to use when registering the node with Consul *(optional)*

+----------------------+------------------------+
| Environment Variable | ``CONSUL_ACL``         |
+----------------------+------------------------+
| Setting Key          | ``consul_acl``         |
+----------------------+------------------------+
| Data type            | ``list``               |
+----------------------+------------------------+
| Default Value        | null                   |
+----------------------+------------------------+

**Consul Service Name**

The name of the service to register with Consul for automatic clustering

+----------------------+-------------------------+
| Environment Variable | ``CONSUL_SERVICE``      |
+----------------------+-------------------------+
| Setting Key          | ``consul_service``      |
+----------------------+-------------------------+
| Data type            | ``list``                |
+----------------------+-------------------------+
| Default Value        | ``rabbitmq``            |
+----------------------+-------------------------+

**Consul Service Prefix**

A prefix that is prepending to the service name when registering with Consul
for automatic clustering *(optional)*

+----------------------+---------------------------+
| Environment Variable | ``CONSUL_SERVICE_PREFIX`` |
+----------------------+---------------------------+
| Setting Key          | ``consul_service_prefix`` |
+----------------------+---------------------------+
| Data type            | ``list``                  |
+----------------------+---------------------------+
| Default Value        | null                      |
+----------------------+---------------------------+

**Consul Service Port**

Used to set a port for the service in Consul, allowing for the automatic clustering
service registration to double as a general RabbitMQ service registration

+----------------------+-------------------------+
| Environment Variable | ``CONSUL_SERVICE_PORT`` |
+----------------------+-------------------------+
| Setting Key          | ``consul_service_port`` |
+----------------------+-------------------------+
| Data type            | ``list``                |
+----------------------+-------------------------+
| Default Value        | ``5672``                |
+----------------------+-------------------------+

.. note:: Set the ``SERVICE_PORT`` to an empty value to disable port announcement
and health checking.  For example: ``SERVICE_PORT=""``

**Consul Service TTL**

Used to specify the Consul health check TTL interval that is used to let Consul
know that RabbitMQ is alive an healthy.

+----------------------+-------------------------+
| Environment Variable | ``CONSUL_SERVICE_TTL``  |
+----------------------+-------------------------+
| Setting Key          | ``consul_service_ttl``  |
+----------------------+-------------------------+
| Data type            | ``list``                |
+----------------------+-------------------------+
| Default Value        | ``30``                  |
+----------------------+-------------------------+

DNS Settings
^^^^^^^^^^^^
The following setting applies only to the DNS backend.

**DNS Hostname**

The FQDN to use when the backend type is ``dns`` for looking up the RabbitMQ nodes to cluster
via a DNS A record round-robin.

+----------------------+------------------------+
| Environment Variable | ``AUTOCLUSTER_HOST``   |
+----------------------+------------------------+
| Setting Key          | ``autocluster_host``   |
+----------------------+------------------------+
| Data type            | ``string``             |
+----------------------+------------------------+
| Default Value        | ``consul``             |
+----------------------+------------------------+

etcd Settings
^^^^^^^^^^^^^
The following settings apply to the etcd backend only.

**etcd URL Scheme**

The URI scheme to use when connecting to etcd

+----------------------+------------------------+
| Environment Variable | ``ETCD_SCHEME``        |
+----------------------+------------------------+
| Setting Key          | ``etcd_scheme``        |
+----------------------+------------------------+
| Data type            | ``list``               |
+----------------------+------------------------+
| Default Value        | ``http``               |
+----------------------+------------------------+

**etcd Host**

The hostname to use when connecting to etcd's API

+----------------------+------------------------+
| Environment Variable | ``ETCD_HOST``          |
+----------------------+------------------------+
| Setting Key          | ``etcd_host``          |
+----------------------+------------------------+
| Data type            | ``list``               |
+----------------------+------------------------+
| Default Value        | ``localhost``          |
+----------------------+------------------------+

**etcd Port**

The port to connect to when using to etcd's API

+----------------------+------------------------+
| Environment Variable | ``ETCD_PORT``          |
+----------------------+------------------------+
| Setting Key          | ``etcd_port``          |
+----------------------+------------------------+
| Data type            | ``int``                |
+----------------------+------------------------+
| Default Value        | ``2379``               |
+----------------------+------------------------+

**etcd Key Prefix**

The prefix used when storing cluster membership keys in etcd

+----------------------+-------------------------+
| Environment Variable | ``ETCD_PREFIX``         |
+----------------------+-------------------------+
| Setting Key          | ``etcd_prefix``         |
+----------------------+-------------------------+
| Data type            | ``list``                |
+----------------------+-------------------------+
| Default Value        | ``rabbitmq``            |
+----------------------+-------------------------+

**etcd Node TTL**

Used to specify how long a node can be down before it is removed from etcd's
list of RabbitMQ nodes in the cluster

+----------------------+-------------------------+
| Environment Variable | ``ETCD_TTL``            |
+----------------------+-------------------------+
| Setting Key          | ``etcd_ttl``            |
+----------------------+-------------------------+
| Data type            | ``integer``             |
+----------------------+-------------------------+
| Default Value        | ``30``                  |
+----------------------+-------------------------+

Logging
^^^^^^^
autocluster will register itself as a configured logger with RabbitMQ if no log
configuration for it exists. To configure logging for the plugin, you add it to
the ``rabbit`` ``log_levels`` configuration like so:

.. code-block:: erlang

    [{rabbit, [
      {log_levels, [{autocluster, debug}, {connection, info}]}
    ]}].

Valid log levels are ``debug``, ``info``, ``warning``, and ``error``. For more
information on configuring RabbitMQ logging, reference the ``log_levels`` key
in the `RabbitMQ documentation <https://www.rabbitmq.com/configure.html>`_.

Example rabbitmq.config
^^^^^^^^^^^^^^^^^^^^^^^
.. code-block:: erlang

    [{autocluster,
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
Steps to custom build a version of the ``rabbitmq-autocluster`` plugin:

.. code-block:: bash

    git clone https://github.com/rabbitmq/rabbitmq-public-umbrella.git
    cd rabbitmq-public-umbrella
    make co
    make BRANCH=rabbitmq_v3_5_4 up_c
    git clone https://github.com/aweber/rabbitmq-autocluster.git
    cd rabbitmq-autocluster
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
the ``aweber/rabbitmq-autocluster`` tag.

Starting the Testing Environment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can run the first container to get access to the management UI with:

.. code-block:: bash

    docker run -d -p 15672:15672 --dns 127.0.0.1 --dns-search node.rmq.consul aweber/rabbitmq-autocluster

The RabbitMQ management UI will the be available at http://192.168.150.10:15672/ and
you can login with the username/password combo of ``guest``/``guest``.

You can run subsequent containers with:

.. code-block:: bash

    docker run -d --dns 127.0.0.1 --dns-search node.rmq.consul aweber/rabbitmq-autocluster

Starting the Development Environment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
For creation of the docker image, you should first compile the plugin from the project directory
and copy the plugin's ez file from ``dist`` to the project docker directory:

.. code-block:: bash

    make
    cp dist/autocluster-0.0.0.ez docker

The build artifact will be copied into the Docker image that is created and enabled
so that it works out of the box. Once copied, build the local image:

.. code-block:: bash

    docker build -t rabbitmq-autocluster /home/core/share/autocluster/docker/

Use the following docker command to start the first container:

.. code-block:: bash

    docker run -d -p 15672:15672 --dns 127.0.0.1 --dns-search node.rmq.consul \
      -v /home/core/share:/opt/rabbitmq-public-umbrella rabbitmq-autocluster

And the following command for all subsequent containers:

.. code-block:: bash

    docker run -d --dns 127.0.0.1 --dns-search node.rmq.consul \
      -v /home/core/share:/opt/rabbitmq-public-umbrella rabbitmq-autocluster

The ``rabbitmq-public-umbrella`` directory is available under ``/opt/source`` in the container.

Development Notes
^^^^^^^^^^^^^^^^^
- If you intend to do development against the containerized cluster, once you get your
  first container up and running you should use ``docker exec`` to connect into it and stop
  RabbitMQ with ``supervisorctl stop rabbitmq``. Then you'll want to run RabbitMQ out of the
  ``/opt/source/rabbitmq-public-umbrella/rabbitmq-server`` directory by running
  ``make run``. Then you can do the same in each container you want to work with.
- When you compile the source in the ``rabbitmq-autocluster`` directory
  on your host machine, all of the containers will see the change in their
  ``/opt/source/rabbitmq-public-umbrella/rabbitmq-autocluster`` directory.
