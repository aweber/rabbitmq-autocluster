%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================

-record(config, {key, os, default, type}).

-define(CONFIG_MAP,
        [{config, backend,              "AUTOCLUSTER_TYPE",      consul,      atom},
         {config, autocluster_host,     "AUTOCLUSTER_HOST",      "undefined", string},
         {config, longname,             "RABBITMQ_LONGNAME",     false,       atom},
         {config, cluster_name,         "CLUSTER_NAME",          "undefined", string},
         {config, consul_acl,           "CONSUL_ACL",            "undefined", string},
         {config, consul_scheme,        "CONSUL_SCHEME",         "http",      string},
         {config, consul_host,          "CONSUL_HOST",           "localhost", string},
         {config, consul_port,          "CONSUL_PORT",           8500,        integer},
         {config, consul_service,       "CONSUL_SERVICE",        "rabbitmq",  string},
         {config, consul_service_port,  "CONSUL_SERVICE_PORT",   5672,        integer},
         {config, consul_service_prefix,"CONSUL_SERVICE_PREFIX", "undefined", string},
         {config, consul_service_ttl,   "CONSUL_SERVICE_TTL",    30,          integer},
         {config, etcd_scheme,          "ETCD_SCHEME",           "http",      string},
         {config, etcd_host,            "ETCD_HOST",             "localhost", string},
         {config, etcd_port,            "ETCD_PORT",             2379,        integer},
         {config, etcd_prefix,          "ETCD_PREFIX",           "rabbitmq",  string},
         {config, etcd_node_ttl,        "ETC_NODE_TTL",          30,          integer}]).

-define(CONSUL_CHECK_NOTES, list_to_atom("RabbitMQ Auto-Cluster Plugin TTL Check")).
