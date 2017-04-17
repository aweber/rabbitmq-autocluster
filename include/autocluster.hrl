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
         {config, lock_wait_time,        "LOCK_WAIT_TIME",         300,          integer, false},
         {config, cluster_cleanup,       "AUTOCLUSTER_CLEANUP",    false,        atom,    false},
         {config, autocluster_log_level, "AUTOCLUSTER_LOG_LEVEL",  info,         atom,    false},
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
         {config, consul_svc,            "CONSUL_SVC",             "rabbitmq",   string,  false},
         {config, consul_svc_addr,       "CONSUL_SVC_ADDR",        "undefined",  string,  false},
         {config, consul_svc_addr_auto,  "CONSUL_SVC_ADDR_AUTO",   false,        atom,    false},
         {config, consul_svc_addr_nic,   "CONSUL_SVC_ADDR_NIC",    "undefined",  string,  false},
         {config, consul_svc_port,       "CONSUL_SVC_PORT",        5672,         integer, true},
         {config, consul_svc_ttl,        "CONSUL_SVC_TTL",         30,           integer, false},

         {config, autocluster_host,      "AUTOCLUSTER_HOST",       "undefined",  string,  false}, %% DNS

         {config, k8s_scheme,            "K8S_SCHEME",             "https",      string,  false}, %% kubernetes
         {config, k8s_host,              "K8S_HOST",               "kubernetes.default.svc.cluster.local",
                                                                                 string,  false},
         {config, k8s_port,              "K8S_PORT",               443,          integer, true},
         {config, k8s_token_path,        "K8S_TOKEN_PATH",         "/var/run/secrets/kubernetes.io/serviceaccount/token",
                                                                                 string,  false},
         {config, k8s_cert_path,         "K8S_CERT_PATH",          "/var/run/secrets/kubernetes.io/serviceaccount/ca.crt",
                                                                                 string,  false},
         {config, k8s_namespace_path,    "K8S_NAMESPACE_PATH",     "/var/run/secrets/kubernetes.io/serviceaccount/namespace",
                                                                                 string,  false},
         {config, k8s_service_name,      "K8S_SERVICE_NAME",       "rabbitmq",   string,  false},

         {config, etcd_scheme,           "ETCD_SCHEME",            "http",       string,  false}, %% etcd
         {config, etcd_host,             "ETCD_HOST",              "localhost",  string,  false},
         {config, etcd_port,             "ETCD_PORT",              2379,         integer, true},
         {config, etcd_prefix,           "ETCD_PREFIX",            "rabbitmq",   string,  false},
         {config, etcd_node_ttl,         "ETCD_NODE_TTL",          30,           integer, false}]).

-define(CONSUL_CHECK_NOTES, "RabbitMQ Auto-Cluster Plugin TTL Check").

%%--------------------------------------------------------------------
%% @doc
%% State that is being passed around and updated by different
%% initialization steps.
%% @end
%%--------------------------------------------------------------------
-record(startup_state, {backend_name :: atom()
                       ,backend_module :: module()
                       ,best_node_to_join :: undefined | node()
                       ,startup_lock_data = undefined
                       }).

%%--------------------------------------------------------------------
%% @doc
%% Detailed node information neccessary for choosing the best cluster
%% to join to.
%% @end
%%--------------------------------------------------------------------
-record(augmented_node, {name :: node()
                        ,uptime :: non_neg_integer()
                        ,alive :: boolean()
                        ,clustered_with :: [node()]
                        ,alive_cluster_nodes :: [node()]
                        ,partitioned_cluster_nodes :: [node()]
                        ,other_cluster_nodes :: [node()]
                        }).
