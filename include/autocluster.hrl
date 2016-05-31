%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @end
%%==============================================================================

-record(config, {key, os, default, type, is_port}).

%% Config Record  key                    environment variable      default       type     is port
-define(CONFIG_MAP,
        [{config, backend,               "AUTOCLUSTER_TYPE",       unconfigured, atom,    false}, %% General
         {config, autocluster_failure,   "AUTOCLUSTER_FAILURE",    ignore,       atom,    false},
         {config, startup_delay,         "AUTOCLUSTER_DELAY",      5,            integer, false},
         {config, cluster_cleanup,       "AUTOCLUSTER_CLEANUP",    false,        atom,    false},
         {config, cleanup_interval,      "CLEANUP_INTERVAL",       60,           integer, false},
         {config, cleanup_warn_only,     "CLEANUP_WARN_ONLY",      true,         atom,    false},
         {config, longname,              "RABBITMQ_USE_LONGNAME",  false,        atom,    false},
         {config, node_name,             "RABBITMQ_NODENAME",      "rabbit",     string,  false},
         {config, node_type,             "RABBITMQ_NODE_TYPE",     disc,         atom,    false},

         {config, aws_autoscaling,       "AWS_AUTOSCALING",        false,        atom,    false}, %% AWS
         {config, aws_ec2_tags,          "AWS_EC2_TAGS",           "undefined",  string,  false},
         {config, aws_access_key,        "AWS_ACCESS_KEY_ID",      "undefined",  string,  false},
         {config, aws_secret_key,        "AWS_SECRET_ACCESS_KEY",  "undefined",  string,  false},
         {config, aws_ec2_region,        "AWS_DEFAULT_REGION",     "undefined",  string,  false},

         {config, cluster_name,          "CLUSTER_NAME",           "undefined",  string,  false}, %% Consul && etcd

         {config, consul_acl_token,      "CONSUL_ACL_TOKEN",       "undefined",  string,  false}, %% Consul
         {config, consul_scheme,         "CONSUL_SCHEME",          "http",       string,  false},
         {config, consul_host,           "CONSUL_HOST",            "localhost",  string,  false},
         {config, consul_port,           "CONSUL_PORT",            8500,         integer, true},
         {config, consul_service,        "CONSUL_SERVICE",         "rabbitmq",   string,  false},
         {config, consul_service_ttl,    "CONSUL_SERVICE_TTL",     30,           integer, false},
         {config, consul_service_address,"CONSUL_SVC_ADDR",        "undefined",  string,  false},
         {config, consul_auto_address,   "CONSUL_SVC_ADDR_AUTO",   false,        atom,    false},
         {config, consul_service_port,   "CONSUL_SVC_PORT",        5672,         integer, true},

         {config, autocluster_host,      "AUTOCLUSTER_HOST",       "undefined",  string,  false}, %% DNS

         {config, etcd_scheme,           "ETCD_SCHEME",            "http",       string,  false}, %% etcd
         {config, etcd_host,             "ETCD_HOST",              "localhost",  string,  false},
         {config, etcd_port,             "ETCD_PORT",              2379,         integer, true},
         {config, etcd_prefix,           "ETCD_PREFIX",            "rabbitmq",   string,  false},
         {config, etcd_node_ttl,         "ETCD_NODE_TTL",          30,           integer, false}]).

-define(CONSUL_CHECK_NOTES, "RabbitMQ Auto-Cluster Plugin TTL Check").
