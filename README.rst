RabbitMQ Autocluster
====================
An RabbitMQ plugin that clusters nodes automatically using `Consul <https://consul.io>`_,
`etcd2 <https://github.com/coreos/etcd>`_, DNS, `AWS EC2 tags <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html>`_
or `AWS Autoscaling Groups <https://aws.amazon.com/autoscaling/>`_ for service discovery.

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
Place the plugin in the RabbitMQ plugins directory. To enable,
run ``rabbitmq-plugins enable autocluster``.

Configuration
-------------
Configuration documentation can be found in the
[Project Wiki](https://github.com/aweber/rabbitmq-autocluster/wiki).

Development
-----------
Instructions for setting up the development environment can be found in the
[Project Wiki](https://github.com/aweber/rabbitmq-autocluster/wiki/Development-Environment).

License
-------
BSD 3-Clause
