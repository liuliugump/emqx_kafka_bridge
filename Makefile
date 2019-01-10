PROJECT = emqttd_kafka_bridge
PROJECT_DESCRIPTION = MQTT-KAFKA
PROJECT_VERSION = 3.0

DEPS = brod
dep_brod = git https://github.com/klarna/brod.git master

BUILD_DEPS = emqx cuttlefish supervisor3 kafka_protocol
dep_emqx = git-emqx https://github.com/emqx/emqx master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish
dep_supervisor3 = git https://github.com/klarna/supervisor3.git
dep_kafka_protocol = git https://github.com/klarna/kafka_protocol.git

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqttd_kafka_bridge.conf -i priv/emqttd_kafka_bridge.schema -d data
