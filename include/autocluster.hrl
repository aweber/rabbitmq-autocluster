%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================

-record(config, {key, os, default, type, is_port}).

-define(CONFIG_MAP,
        [{config, backend,              "AUTOCLUSTER_TYPE",      consul,      atom,    false},
         {config, autocluster_host,     "AUTOCLUSTER_HOST",      "undefined", string,  false},
         {config, longname,             "RABBITMQ_USE_LONGNAME", false,       atom,    false},
         {config, node_type,            "RABBITMQ_NODE_TYPE",    disc,        atom,    false},
         {config, cluster_name,         "CLUSTER_NAME",          "undefined", string,  false},
         {config, consul_acl,           "CONSUL_ACL",            "undefined", string,  false},
         {config, consul_scheme,        "CONSUL_SCHEME",         "http",      string,  false},
         {config, consul_host,          "CONSUL_HOST",           "localhost", string,  false},
         {config, consul_port,          "CONSUL_PORT",           8500,        integer, true},
         {config, consul_service,       "CONSUL_SERVICE",        "rabbitmq",  string,  false},
         {config, consul_service_port,  "CONSUL_SERVICE_PORT",   5672,        integer, true},
         {config, consul_service_prefix,"CONSUL_SERVICE_PREFIX", "undefined", string,  false},
         {config, consul_service_ttl,   "CONSUL_SERVICE_TTL",    30,          integer, false},
         {config, etcd_scheme,          "ETCD_SCHEME",           "http",      string,  false},
         {config, etcd_host,            "ETCD_HOST",             "localhost", string,  false},
         {config, etcd_port,            "ETCD_PORT",             2379,        integer, true},
         {config, etcd_prefix,          "ETCD_PREFIX",           "rabbitmq",  string,  false},
         {config, etcd_node_ttl,        "ETC_NODE_TTL",          30,          integer, false}]).

-define(CONSUL_CHECK_NOTES, list_to_atom("RabbitMQ Auto-Cluster Plugin TTL Check")).
