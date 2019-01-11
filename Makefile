PROJECT = emqx_kafka_bridge
PROJECT_DESCRIPTION = MQTT-KAFKA
PROJECT_VERSION = 3.0

DEPS = brod
dep_brod = git https://github.com/klarna/brod.git master

BUILD_DEPS = emqx cuttlefish supervisor3 kafka_protocol
dep_emqx = git-emqx https://github.com/emqx/emqx master
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.0
dep_supervisor3 = git https://github.com/klarna/supervisor3.git
dep_kafka_protocol = git https://github.com/klarna/kafka_protocol.git

ERLC_OPTS += +debug_info

NO_AUTOPATCH = cuttlefish

COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_kafka_bridge.conf -i priv/emqx_kafka_bridge.schema -d data