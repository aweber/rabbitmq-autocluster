PROJECT = autocluster

DEPS = rabbit rabbitmq_aws
TEST_DEPS = rabbit
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

NO_AUTOPATCH += rabbitmq_aws

dep_rabbitmq_aws = git https://github.com/gmr/rabbitmq-aws.git master

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

current_rmq_ref = stable

include rabbitmq-components.mk
include erlang.mk


# --------------------------------------------------------------------
# Testing.
# --------------------------------------------------------------------

WITH_BROKER_TEST_COMMANDS := autocluster_all_tests:run()
