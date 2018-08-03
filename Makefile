PROJECT = emqttd_kafka_bridge
PROJECT_DESCRIPTION = EMQTTD Kafka Bridge
PROJECT_VERSION = 2.0.7

DEPS = brod
dep_brod = git https://github.com/klarna/brod.git master


BUILD_DEPS = emqttd cuttlefish supervisor3 kafka_protocol
dep_emqttd = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish
dep_supervisor3 = git https://github.com/klarna/supervisor3.git
dep_kafka_protocol = git https://github.com/klarna/kafka_protocol.git

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqttd_kafka_bridge.conf -i priv/emqttd_kafka_bridge.schema -d data
