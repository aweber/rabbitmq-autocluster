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

You can change the settings for the plugin in the ``rabbitmq.config`` file under
a ``rabbitmq_autocluster_consul`` stanza. The configuration can change any of these
default values:

+----------------+--------------------------------------+-----------+---------------+
| Setting        | Description                          | Data Type | Default Value |
+================+======================================+===========+===============+
| consul_host    | The Consul client host to use        | list      | ``localhost`` |
+----------------+--------------------------------------+-----------+---------------+
| consul_port    | The port to connect on               | integer   | ``8500``      |
+----------------+--------------------------------------+-----------+---------------+
| consul_acl     | The Consul ACL to use for requests   | list      | Unset         |
+----------------+--------------------------------------+-----------+---------------+
| consul_service | The Consul service name to register  | list      | ``rabbitmq``  |
+----------------+--------------------------------------+-----------+---------------+
| cluster_name   | The name of the RabbitMQ cluster to  | list      | Unset         |
|                | restrict membership to               |           |               |
+----------------+--------------------------------------+-----------+---------------+

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
Steps to custom build a version of the rabbitmq-autocluster-consul plugin:

.. code-block:: bash

    git clone https://github.com/rabbitmq/rabbitmq-public-umbrella.git
    cd rabbitmq-public-umbrella
    make co
    make BRANCH=rabbitmq_v3_5_1 up_c
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
