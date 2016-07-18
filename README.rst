RabbitMQ Autocluster
====================
A RabbitMQ plugin that clusters nodes automatically using `Consul <https://consul.io>`_,
`etcd2 <https://github.com/coreos/etcd>`_, DNS, `AWS EC2 tags <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html>`_
or `AWS Autoscaling Groups <https://aws.amazon.com/autoscaling/>`_ for service discovery.

**Note** This plugin is not a replacement for first-hand knowledge of how to manually create a RabbitMQ cluster. If you run into issues using the plugin, you should try and manually create the cluster in the same environment as you are trying to use the plugin in. For information on how to cluster RabbitMQ manually, please see the `RabbitMQ documentation <https://www.rabbitmq.com/clustering.html>`_.

.. image:: https://img.shields.io/travis/aweber/rabbitmq-autocluster.svg
    :target: https://travis-ci.org/aweber/rabbitmq-autocluster
.. image:: https://img.shields.io/github/release/aweber/rabbitmq-autocluster.svg
    :target: https://github.com/aweber/rabbitmq-autocluster/releases

Download
--------
Downloads of autocluster can be found on the
`GitHub Releases <https://github.com/aweber/rabbitmq-autocluster/releases>`_ page.

Check for version compatibility in the release notes.

**Important**
The ``rabbitmq_autocluster`` plugin requires Erlang R17.5 or higher. *This may not correspond with the minimum Erlang
version required by RabbitMQ.*

Installation
------------

1. Place both ``autocluster-%%VSN%%.ez`` and the ``rabbitmq_aws-%%VSN%%.ez`` plugin files in the RabbitMQ plugins directory.
2. Run ``rabbitmq-plugins enable autocluster``.
3. Configure the plugin.

Alternatively, there is a pre-built Docker Image available at on DockerHub as `aweber/rabbitmq-autocluster <https://hub.docker.com/r/aweber/rabbitmq-autocluster/>`_.

**Note**
As of version ``0.5`` the autocluster plugin does not have a default backend configured. See the `Project Wiki <https://github.com/aweber/rabbitmq-autocluster/wiki>`_ for configuration details.

Configuration
-------------
Configuration documentation can be found in the
`Project Wiki <https://github.com/aweber/rabbitmq-autocluster/wiki>`_.

Development
-----------
Instructions for setting up the development environment can be found in the
`Project Wiki <https://github.com/aweber/rabbitmq-autocluster/wiki/Development-Environment>`_.

License
-------
BSD 3-Clause
