RabbitMQ Autocluster
====================

A RabbitMQ plugin that clusters nodes automatically using a number of peer discovery mechanisms:

* [Consul](https://consul.io>),
* [etcd2](https://github.com/coreos/etcd),
* DNS A records,
* [AWS EC2 tags](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html),
* [AWS Autoscaling Groups](https://aws.amazon.com/autoscaling/).

**Note:** This plugin is not a replacement for first-hand knowledge of
how to manually create a RabbitMQ cluster. If you run into issues
using the plugin, you should try and manually create the cluster in
the same environment as you are trying to use the plugin in. For
information on how to cluster RabbitMQ manually, please see the
[RabbitMQ documentation](https://www.rabbitmq.com/clustering.html).

Current Maintainers
-------------------

This plugin was originally developed by [Gavin Roy](https://github.com/gmr) at AWeber
and is now co-maintained by several RabbitMQ core contributors.
Parts of it were adopted into RabbitMQ core (as of 3.7.0).


Supported RabbitMQ Versions
---------------------------

There are two branches in this repository that target different RabbitMQ
release series:

* [stable](https://github.com/rabbitmq/rabbitmq-autocluster/tree/stable) targets RabbitMQ ``3.6.x`` (current ``stable`` RabbitMQ branch)
* [master](https://github.com/rabbitmq/rabbitmq-autocluster/tree/master) targets RabbitMQ ``3.7.x`` (current ``master`` RabbitMQ branch)

Please take this into account when building this plugin from source.


Supported Erlang Versions
-------------------------

This plugin requires Erlang/OTP 17.5 or later.
Also see [RabbitMQ Erlang version requirements](http://next.rabbitmq.com/which-erlang.html) guide, too.


Binary Releases
---------------

Binary releases of autocluster can be found on the
[GitHub Releases](https://github.com/aweber/rabbitmq-autocluster/releases) page.

![https://github.com/rabbitmq/rabbitmq-autocluster/releases](https://img.shields.io/github/release/rabbitmq/rabbitmq-autocluster.svg)

Check for version compatibility in the release notes.


Installation
------------

1. Place both ``autocluster-%%VSN%%.ez`` and the ``rabbitmq_aws-%%VSN%%.ez`` plugin files in the RabbitMQ plugins directory.
2. Run ``rabbitmq-plugins enable autocluster``.
3. Configure the plugin.

Alternatively, there is a pre-built Docker Image available at on DockerHub as [aweber/rabbitmq-autocluster](https://hub.docker.com/r/aweber/rabbitmq-autocluster/).

**Note**
As of version ``0.5`` the autocluster plugin does not have a default backend configured. See the [Project Wiki](https://github.com/aweber/rabbitmq-autocluster/wiki) for configuration details.

Configuration
-------------

  * [General settings](#general-settings)
     * [Available Settings](#available-settings)
     * [Details](#details)
     * [AWS API Configuration and Credentials](#aws-api-configuration-and-credentials)
     * [AWS Credentials and Configuration Settings](#aws-credentials-and-configuration-settings)
     * [IAM Policy](#iam-policy)
     * [Example Configuration](#example-configuration)
     * [Example Cloud-Init](#example-cloud-init)
  * [Consul configuration](#consul-configuration)
     * [Configuration Details](#configuration-details)
     * [Example rabbitmq.config](#example-rabbitmqconfig)
  * [DNS configuration](#dns-configuration)
     * [Example Configuration](#example-configuration-1)
     * [Troubleshooting](#troubleshooting)
  * [etcd configuration](#etcd-configuration)
  * [K8S configuration](#k8s-configuration)
     * [Kubernetes Setup](#kubernetes-setup)

### General settings

Configuration for the plugin can be set in two places: operating system environment variables
or the ``rabbitmq.config`` file under the ``autocluster`` stanza.

#### Available Settings

The following settings are available for all service discovery backends:

<dl>
  <dt>Backend Type</dt>
  <dd>Which type of service discovery backend to use. One of <code>aws</code>, <code>consul</code>, <code>dns</code>, or <code>etcd</code>.</dd>
  <dt>Startup Delay</dt>
  <dd>To prevent a race condition when creating a new cluster for the first time, the startup delay performs a random sleep that should cause nodes to start in a slightly random offset from each other. The setting lets you control the maximum value for the startup delay.</dd>
  <dt>Failure Mode</dt>
  <dd>What behavior to use when the node fails to cluster with an existing RabbitMQ cluster or during initialization of the autocluster plugin. The two valid options are <code>ignore</code> and <code>stop</code>.</dd>
  <dt>Log Level</dt>
  <dd>Prior to <em>v0.6</em>, log level can be set using RabbitMQ's <code>log_levels</code> configuration - as described at the end of this page. Starting with <em>v0.6</em> you can set the log level via the environment variable <code>AUTOCLUSTER_LOG_LEVEL</code> or the <code>autocluster_log_level</code> setting of the <code>autocluster</code> application.</dd>
  <dt>Longname (FQDN) Support</dt>
  <dd>This is a RabbitMQ  environment variable setting that is used by the autocluster plugin as well. When set to <code>true</code> this will cause RabbitMQ <b>and the</b> autocluster plugin to use fully qualified names to identify nodes. For more information about the <code>RABBITMQ_USE_LONGNAME</code> environment variable, see the <a href="https://www.rabbitmq.com/configure.html#define-environment-variables">RabbitMQ documentation</a></dd>
  <dt>Node Name</dt>
  <dd>Like Longname Support, Node Name is a RabbitMQ setting that is used by the autocluster plugin as well. When set to <code>true</code> this will cause RabbitMQ <b>and the</b> autocluster plugin. The <code>RABBITMQ_NODENAME</code> environment variable explicitly sets the node name that is used to identify the node with RabbitMQ. The autocluster plugin will use this value when constructing the local part/name/prefix for all nodes in this cluster. For example, if <code>RABBITMQ_NODENAME</code> is set to <code>bunny@rabbit1</code>, <code>bunny</code> will be prefixed to all nodes discovered by the various backends. For more information about the <code>RABBITMQ_NODENAME</code> environment variable, see the <a href="https://www.rabbitmq.com/configure.html#define-environment-variables">RabbitMQ documentation</a></dd>
  <dt>Node Type</dt>
  <dd>Define the type of node to join the cluster as. One of <code>disc</code> or <code>ram</code>. See the <a href="https://www.rabbitmq.com/clustering.html">RabbitMQ Clustering Guide</a> for more information.</dd>
  <dt>Cluster Cleanup</dt>
  <dd>Enables a periodic check that removes any nodes that are not alive in the cluster and no longer listed in the service discovery list. This is a destructive action that removes nodes from the cluster. Nodes that are flapping and removed will be re-added as if they were coming in new and their database, including any persisted messages will be gone. To use this feature, you must not only enable it with this flag, <b>but also disable the "Cleanup Warn Only" flag</b>. <em>Added in v0.5</em>

<em>Note</em>: This is an experimental feature and should be used with caution.</dd>
  <dt>Cleanup Interval</dt>
  <dd>If cluster cleanup is enabled, this is the interval that specifies how often to look for dead nodes to remove (in seconds). <em>Added in v0.5</em></dd>
  <dt>Cleanup Warn Only</dt>
  <dd>If set, the plugin will only warn about nodes that it would cleanup and will not perform any destructive actions on the cluster. <em>Added in v0.5</em></dd>
  <dl/>

#### Settings Details

You are able to configure autocluster plugin via Environment Variables or in the [rabbitmq.config](https://www.rabbitmq.com/configure.html#configuration-file) file.

**Note**: RabbitMQ reads its own config file with environment variables - ``rabbitmq-env.conf``, but you can't easily reuse it for ``autocluster`` configuration. If you absolutely want to do it, you should use ``export VAR_NAME=var_value`` instead of a plain assignment to ``VAR_NAME``.

The following chart details each general setting, with the environment variable name, ``rabbitmq.config`` setting key and data type, and the default value if there is one.

| Setting           | Environment Variable      | Setting Key               | Type        | Default              |
|-------------------|---------------------------|---------------------------|-------------|----------------------|
| Backend Type      | ``AUTOCLUSTER_TYPE``      | ``backend``               | ``atom``    | ``unconfigured``     |
| Startup Delay     | ``AUTOCLUSTER_DELAY``     | ``startup_delay``         | ``integer`` | ``5``                |
| Failure Mode      | ``AUTOCLUSTER_FAILURE``   | ``autocluster_failure``   | ``atom``    | ``ignore``           |
| Log Level         | ``AUTOCLUSTER_LOG_LEVEL`` | ``autocluster_log_level`` | ``atom``    | ``info``             |
| Longname          | ``RABBITMQ_USE_LONGNAME`` |                           | ``bool``    | ``false``            |
| Node Name         | ``RABBITMQ_NODENAME``     |                           | ``string``  | ``rabbit@$HOSTNAME`` |
| Node Type         | ``RABBITMQ_NODE_TYPE``    | ``node_type``             | ``atom``    | ``disc``             |
| Cluster Cleanup   | ``AUTOCLUSTER_CLEANUP``   | ``cluster_cleanup``       | ``bool``    | ``false``            |
| Cleanup Interval  | ``CLEANUP_INTERVAL``      | ``cleanup_interval``      | ``integer`` | ``60``               |
| Cleanup Warn Only | ``CLEANUP_WARN_ONLY``     | ``cleanup_warn_only``     | ``bool``    | ``true``             |

#### Logging Configuration prior to *v0.6*

This is the only way to enable debug logging for the plugin for versions prior to v0.6. Since v0.6 it's no longer effective, you should use ``AUTOCLUSTER_LOG_LEVEL`` environment variable or ``autocluster_log_level`` setting.

autocluster will register itself as a configured logger with RabbitMQ if no log configuration for it exists. To configure logging for the plugin, you add it to the ``rabbit`` ``log_levels`` configuration like so:

```erlang

[{rabbit, [
           {log_levels, [{autocluster, debug}, {connection, info}]}
                        ]}].
```

Valid log levels are ``debug``, ``info``, ``warning``, and ``error``. For more information on configuring RabbitMQ logging, reference the ``log_levels`` key in the [RabbitMQ documentation](https://www.rabbitmq.com/configure.html).

### AWS Configuration

The AWS backend for the autocluster supports two different node discovery, [Autoscaling Group](https://aws.amazon.com/autoscaling/) membership and [EC2 tags](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html).

The following settings impact the behavior of the AWS backend. See the AWS API Credentials section below for additional settings.

<dl>
  <dt>Autoscaling</dt>
  <dd>Cluster based upon membership in an Autoscaling Group. Set to <code>true</code> to enable.</dd>
  <dt>EC2 Tags</dt>
  <dd>Filter the cluster node list with the specified tags. Use a comma delimiter for multiple tags when specifying as an environment variable.</dd>
</dl>

**NOTE**: If this is your first time setting up RabbitMQ with the autoscaling cluster and are doing so for R&D purposes, you may want to check out the [gavinmroy/alpine-rabbitmq-autocluster](https://github.com/gmr/alpine-rabbitmq-autocluster) Docker Image repository for a working example of the plugin using a CloudFormation template that creates everything required for an Autoscaling Group based cluster.

#### Details

| Environment Variable | Setting Key         | Type           | Default   |
|----------------------|---------------------|----------------|-----------|
| ``AWS_AUTOSCALING``  | ``aws_autoscaling`` | ``atom``       | ``false`` |
| ``AWS_EC2_TAGS``     | ``aws_ec2_tags``    | ``[string()]`` |           |

Notes
'''''

If ``aws_autoscaling`` is enabled, the EC2 backend will dynamically determine the autoscaling group that the node is a member of and attempt to join the other nodes in the autoscaling group.

If ``aws_autoscaling`` is disabled, you must specify EC2 tags to use to filter the nodes that the backend should cluster with.

#### AWS API Configuration and Credentials

As with the [AWS CLI](http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-getting-started.html), the ``autocluster`` plugin configures the AWS API requests by attempting to resolve the values in a number of steps.

The configuration values are discovered in the following order:

1. Explicitly configured in the ``autocluster`` configuration.
2. Environment variables
3. Configuration file
4. EC2 Instance Metadata Service (for Region)

The credentials values are discovered in the following order:

1. Explicitly configured in the ``autocluster`` configuration.
2. Environment variables
3. Credentials file
4. EC2 Instance Metadata Service

#### AWS Credentials and Configuration Settings

The following settings and environment variables impact the configuration and credentials behavior. For more information see the [Amazon AWS CLI documentation](http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-getting-started.html).

| Environment Variable            | Setting Key        | Type       | Default       |
|---------------------------------|--------------------|------------|---------------|
| ``AWS_ACCESS_KEY_ID``           | ``aws_access_key`` | ``string`` |               |
| ``AWS_SECRET_ACCESS_KEY``       | ``aws_secret_key`` | ``string`` |               |
| ``AWS_DEFAULT_REGION``          | ``aws_ec2_region`` | ``string`` | ``us-east-1`` |
| ``AWS_DEFAULT_PROFILE``         | N/A                | ``string`` |               |
| ``AWS_CONFIG_FILE``             | N/A                | ``string`` |               |
| ``AWS_SHARED_CREDENTIALS_FILE`` | N/A                | ``string`` |               |

#### IAM Policy

If you intend to use the EC2 Instance Metadata Service along with an IAM Role that is assigned to EC2 instances, you will need a policy that allows the plugin to discover the node list. The following is an example of such a policy:

```json
{
"Version": "2012-10-17",
"Statement": [
              {
              "Effect": "Allow",
              "Action": [
                         "autoscaling:DescribeAutoScalingInstances",
                         "ec2:DescribeInstances"
                         ],
              "Resource": [
                           "*"
                           ]
              }
              ]
}
```

#### Example Configuration

The following configuration example enables the autoscaling based cluster discovery and sets the EC2 region to ``us-west-2``:

```erlang
[
 {rabbit, [
           {log_levels, [{autocluster, info}, {connection, info}]}
                        ]},
                         {autocluster, [
                                        {backend, aws},
                                                  {aws_autoscaling, true},
                                                  {aws_ec2_region, "us-west-2"}
                                                  ]}
                                                   ].
```

For non-autoscaling group based clusters, the following configuration demonstrates how to limit EC2 instances in the cluster to nodes with the tags ``region=us-west-2`` and ``service=rabbitmq``. It also specifies the AWS access key and AWS secret key.

```erlang
[
 {rabbit, [
           {log_levels, [{autocluster, info}, {connection, info}]}
                        ]},
                         {autocluster, [
                                        {backend, aws},
                                                  {aws_ec2_tags, [{"region", "us-west-2"}, {"service", "rabbitmq"}]},
                                                  {aws_ec2_region, "us-east-1"},
                                                  {aws_access_key, "AKIDEXAMPLE"},
                                                  {aws_secret_key, "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"}
                                                  ]}
                                                   ].
```

When using environment variables, the tags must be provided in JSON format:
```
AWS_EC2_TAGS="{\"region\": \"us-west-2\",\"service\": \"rabbitmq\"}"
```

#### Example Cloud-Init

The following is an example cloud-init that was tested with Ubuntu Trusty for use with an Autoscaling Group:

```yaml
#cloud-config
apt_update: true
apt_upgrade: true
apt_sources:
- source: deb https://apt.dockerproject.org/repo ubuntu-trusty main
keyid: 58118E89F3A912897C070ADBF76221572C52609D
filename: docker.list
packages:
- docker-engine
runcmd:
- docker run -d --name rabbitmq --net=host -p 4369:4369 -p 5672:5672 -p 15672:15672 -p 25672:25672 gavinmroy/rabbitmq-autocluster
```                                    
### Consul configuration

The following settings impact the configuration of the [Consul](http://consul.io) backend for the autocluster plugin:

<dl>
  <dt>Consul Scheme</dt>
  <dd>The URI scheme to use when connecting to Consul</dd>
  <dt>Consul Host</dt>
  <dd>The hostname to use when connecting to Consul's API</dd>
  <dt>Consul Port</dt>
  <dd>The port to use when connecting to Consul's API</dd>
  <dt>Consul ACL Token</dt>
  <dd>The Consul access token to use when registering the node with Consul <em>(optional)</em></dd>
  <dt>Service Name</dt>
  <dd>The name of the service to register with Consul for automatic clustering</dd>
  <dt>Service Address</dt>
  <dd>An IP address or host name to use when registering the service. This is useful when you are testing with a single Consul server instead of having an agent for every RabbitMQ node. If this is specified, the value will automatically be appended to the service ID. <em>(optional) - Added in v0.5</em></dd>
  <dt>Service Auto Address</dt>
  <dd>Use the hostname of the current machine for the service address when registering the service with Consul. This is useful when you are testing with a single Consul server instead of having an agent for every RabbitMQ node. If this is enabled, the hostname will automatically be appended to the service ID. <em>(optional) - Added in v0.5</em></dd>
  <dt>Service Auto Address by NIC</dt>
  <dd>Use the IP address of the specified network interface controller (NIC) as the service address when registering with Consul. <em>(optional) - Added in v0.6</em></dd>
  <dt>Service Port</dt>
  <dd>Used to set a port for the service in Consul, allowing for the automatic clustering service registration to double as a general RabbitMQ service registration.

<b>Note</b>: Set the <code>CONSUL_SVC_PORT</code> to an empty value to disable port announcement and health checking.  For example: <code>CONSUL_SVC_PORT=""</code></dd>
  <dt>Consul Use Longname</dt>
  <dd>When node names are registered with Consul, instead of FQDN's addresses, this option allows to append <em>.node.<consul_domain></em> to the node names retrieved from Consul.</dd>
  <dt>Consul Domain</dt>
  <dd>The domain name appended when Consul longnames are used.</dd>
  <dt>Service TTL</dt>
  <dd>Used to specify the Consul health check TTL interval that is used to let Consul know that RabbitMQ is alive an healthy.</dd>
</dl>

#### Configuration Details

| Setting               | Environment Variable      | Setting Key                | Type        | Default       |
|-----------------------|---------------------------|----------------------------|-------------|---------------|
| Consul Scheme         | ``CONSUL_SCHEME``         | ``consul_scheme``          | ``string``  | ``http``      |
| Consul Host           | ``CONSUL_HOST``           | ``consul_host``            | ``string``  | ``localhost`` |
| Consul Port           | ``CONSUL_PORT``           | ``consul_port``            | ``integer`` | ``8500``      |
| Consul ACL Token      | ``CONSUL_ACL_TOKEN``      | ``consul_acl_token``       | ``string``  |               |
| Service Name          | ``CONSUL_SVC``            | ``consul_svc``             | ``string``  | ``rabbitmq``  |
| Service Address       | ``CONSUL_SVC_ADDR``       | ``consul_svc_addr``        | ``string``  |               |
| Service Auto Address  | ``CONSUL_SVC_ADDR_AUTO``  | ``consul_svc_addr_auto``   | ``boolean`` | ``false``     |
| Service Auto Address by NIC  | ``CONSUL_SVC_ADDR_NIC``   | ``consul_svc_addr_nic``    | ``string``  |               |
| Service Port          | ``CONSUL_SVC_PORT``       | ``consul_svc_port``        | ``integer`` | ``5672``      |
| Service TTL           | ``CONSUL_SVC_TTL``        | ``consul_svc_ttl``         | ``integer`` | ``30``        |
| Consul Use Longname   | ``CONSUL_USE_LONGNAME``   | ``consul_use_longname``    | ``boolean`` | ``false``     |
| Consul Domain         | ``CONSUL_DOMAIN``         | ``consul_domain``          | ``string``  | ``consul``    |

#### Example rabbitmq.config
```erlang

[{autocluster,
 [
  {backend, consul},
            {consul_host, "localhost"},
            {consul_port, 8500},
            {consul_acl_token, "example-acl-token"},
            {consul_svc, "rabbitmq-test"},
            {cluster_name, "test"}
            ]}
                 ].
```

### DNS configuration

The following setting applies only to the DNS backend:

**DNS Hostname**

The FQDN to use when the backend type is ``dns`` for looking up the RabbitMQ nodes to cluster
via a [DNS A record round-robin](https://en.wikipedia.org/wiki/Round-robin_DNS).

| Environment Variable | ``AUTOCLUSTER_HOST``   |
|----------------------|------------------------|
| Setting Key          | ``autocluster_host``   |
| Data type            | ``string``             |
| Default Value        | ``consul``             |

#### Example Configuration

The following configuration example enables the DNS based cluster discovery and sets the autocluster_host variable to your DNS Round-Robin A record:

```erlang
[
 {rabbit, [
           {log_levels, [{autocluster, debug}, {connection, debug}]}
                        ]},
                         {autocluster, [
                                        {backend, dns},
                                                  {autocluster_host, "YOUR_ROUND_ROBIN_A_RECORD"}
                                                  ]}
                                                   ].
```

#### Troubleshooting

If you are having issues getting your RabbitMQ cluster formed, please check that Erlang can resolve:

- The DNS Round-Robin A Record. Imagine having 3 nodes with IPS 10.0.0.2, 10.0.0.3 and 10.0.0.4

```erlang
> inet_res:lookup("YOUR_ROUND_ROBIN_A_RECORD", in, a).
[{10,0,0,2},{10,0,0,3},{10,0,0,4}]
```

- All the nodes have reverse lookup entries in your DNS server. You should get something similar to this:

```erlang
> inet_res:gethostbyaddr({10,0,0,2}).
{ok,{hostent,"YOUR_REVERSE_LOOKUP_ENTRY",[],
inet,4,
[{10,0,0,2}]}}
```

- Erlang will always receive lowercase DNS names so be careful if you use your /etc/hosts file to resolve the other nodes in the cluster and you use uppercase there as RabbitMQ will get confused and the cluster will not form

### etcd configuration

The following settings apply to the [etcd](https://coreos.com/etcd/docs/latest/) backend only:

**NOTE** The etcd backend only supports etcd v2.

**etcd URL Scheme**

The URI scheme to use when connecting to etcd

| Environment Variable | ``ETCD_SCHEME``        |
|----------------------|------------------------|
| Setting Key          | ``etcd_scheme``        |
| Data type            | ``list``               |
| Default Value        | ``http``               |

**etcd Host**

The hostname to use when connecting to etcd's API

| Environment Variable | ``ETCD_HOST``          |
|----------------------|------------------------|
| Setting Key          | ``etcd_host``          |
| Data type            | ``list``               |
| Default Value        | ``localhost``          |

**etcd Port**

The port to connect to when using to etcd's API

| Environment Variable | ``ETCD_PORT``          |
|----------------------|------------------------|
| Setting Key          | ``etcd_port``          |
| Data type            | ``int``                |
| Default Value        | ``2379``               |

**etcd Key Prefix**

The prefix used when storing cluster membership keys in etcd

| Environment Variable | ``ETCD_PREFIX``         |
|----------------------|-------------------------|
| Setting Key          | ``etcd_prefix``         |
| Data type            | ``list``                |
| Default Value        | ``rabbitmq``            |

**etcd Node TTL**

Used to specify how long a node can be down before it is removed from etcd's
list of RabbitMQ nodes in the cluster

| Environment Variable | ``ETCD_TTL``            |
|----------------------|-------------------------|
| Setting Key          | ``etcd_ttl``            |
| Data type            | ``integer``             |
| Default Value        | ``30``                  |

### K8S configuration

The following settings impact the configuration of the [Kubernetes](http://kubernetes.io) backend for the autocluster plugin:

K8S Scheme
The URI scheme to use when connecting to Kubernetes API server

| Environment Variable | ``K8S_SCHEME``         |
|----------------------|------------------------|
| Setting Key          | ``k8s_scheme``         |
| Data type            | ``string``             |
| Default Value        | ``https``              |

K8S Host
The hostname of the kubernetes API server

| Environment Variable | ``K8S_HOST``                             |
|----------------------|------------------------------------------|
| Setting Key          | ``k8s_host``                             |
| Data type            | ``string``                               |
| Default Value        | ``kubernetes.default.svc.cluster.local`` |

K8S Port
The port ot use when connecting to kubernetes API server

| Environment Variable | ``K8S_PORT``           |
|----------------------|------------------------|
| Setting Key          | ``k8s_port``           |
| Data type            | ``integer``            |
| Default Value        | ``443``                |

K8S Token Path
The token path of the Pod's service account

| Environment Variable | ``K8S_TOKEN_PATH``                                      |
|----------------------|---------------------------------------------------------|
| Setting Key          | ``k8s_token_path``                                      |
| Data type            | ``string``                                              |
| Default Value        | ``/var/run/secrets/kubernetes.io/serviceaccount/token`` |

K8S Cert Path
The path of the service account authentication certificate with the k8s API server

| Environment Variable | ``K8S_CERT_PATH``                                        |
|----------------------|----------------------------------------------------------|
| Setting Key          | ``k8s_cert_path``                                        |
| Data type            | ``string``                                               |
| Default Value        | ``/var/run/secrets/kubernetes.io/serviceaccount/ca.crt`` |

K8S Namespace Path
The path of the service account namespace file

| Environment Variable | ``K8S_NAMESPACE_PATH``                                      |
|----------------------|-------------------------------------------------------------|
| Setting Key          | ``k8s_namespace_path``                                      |
| Data type            | ``string``                                                  |
| Default Value        | ``/var/run/secrets/kubernetes.io/serviceaccount/namespace`` |

K8S Service Name
The rabbitmq service name in Kubernetes

| Environment Variable | ``K8S_SERVICE_NAME``   |
|----------------------|------------------------|
| Setting Key          | ``k8s_service_name``   |
| Data type            | ``string``             |
| Default Value        | ``rabbitmq``           |


#### Kubernetes Setup

In order for this plugin to work, your nodes need to use FQDN. i.e. set ``RABBITMQ_USE_LONGNAME=true`` in your pod

Development
-----------

WIP Notes for dev environment

#### Requirements

- erlang 17.5
- docker-machine
- docker-compose
- make

#### Setup

Startup docker-machine:

```bash
docker-machine create --driver virtualbox default
eval $(docker-machine env)
```

Start client containers:

```bash
docker-compose up -d
```

Development environment
------------------------

Work in Progress

#### Make Commands
- ``test``
- ``run-broker``
- ``shell``
- ``dist``

#### Docker
Building the container:
```bash
docker build -t rabbitmq-autocluster .
```

#### Testing Consul behaviors
Here's the base pattern for how I test against Consul when developing:

```bash
make dist
docker build -t gavinmroy/autocluster .

docker network create rabbitmq_network

docker run --rm -t -i --net=rabbitmq_network --name=consul -p 8500:8500 gavinmroy/alpine-consul:0.5.2-0

docker run --rm -t -i --link=consul --net=rabbitmq_network --name=node0 -e AUTOCLUSTER_TYPE=consul -e CONSUL_HOST=consul -e CONSUL_PORT=8500 -e CONSUL_SERVICE_TTL=60  -e AUTOCLUSTER_CLEANUP=true -e CLEANUP_WARN_ONLY=false -e CONSUL_SVC_ADDR_AUTO=true -p 15672:15672 gavinmroy/autocluster

docker run --rm -t -i --link=node0 --net=rabbitmq_network --name=node1 -e RABBITMQ_NODE_TYPE=ram -e AUTOCLUSTER_TYPE=consul -e CONSUL_HOST=consul -e CONSUL_PORT=8500 -e CONSUL_SERVICE_TTL=60  -e AUTOCLUSTER_CLEANUP=true -e CLEANUP_WARN_ONLY=false -e CONSUL_SVC_ADDR_AUTO=true gavinmroy/autocluster

docker run --rm -t -i --link=node1 --net=rabbitmq_network --name=node2 -e RABBITMQ_NODE_TYPE=ram -e AUTOCLUSTER_TYPE=consul -e CONSUL_HOST=consul -e CONSUL_PORT=8500 -e CONSUL_SERVICE_TTL=60  -e AUTOCLUSTER_CLEANUP=true -e CLEANUP_WARN_ONLY=false -e CONSUL_SVC_ADDR_AUTO=true gavinmroy/autocluster
```

License
-------
BSD 3-Clause
