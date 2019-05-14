PROJECT = emqx_kafka_bridge
PROJECT_DESCRIPTION = MQTT KAFKA BRIDGE
PROJECT_VERSION = 3.1.1

CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)
BRANCH := $(if $(filter $(CUR_BRANCH), master develop), $(CUR_BRANCH), develop)

DEPS = brod 
dep_brod = git-emqx https://github.com/klarna/brod master

BUILD_DEPS = emqx cuttlefish supervisor3 kafka_protocol
dep_emqx = git-emqx https://github.com/emqx/emqx $(BRANCH)
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.1
dep_supervisor3 = git-emqx https://github.com/klarna/supervisor3 1.1.8
dep_kafka_protocol = git-emqx https://github.com/klarna/kafka_protocol 2.2.7

ERLC_OPTS += +debug_info

NO_AUTOPATCH = cuttlefish

COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_kafka_bridge.conf -i priv/emqx_kafka_bridge.schema -d data
