DEPS:=rabbitmq-server
RELEASABLE:=true
STANDALONE_TEST_COMMANDS:=eunit:test([rabbitmq_autocluster_consul_tests],[verbose])
