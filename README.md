Version
-------------

master 适配EMQ v2.3.11 版本
release 适配EMQ v3.0.0 版本

Build the EMQ broker
-------------
1. clone emq-relx project
```	
git clone https://github.com/emqtt/emq-relx.git v2.3.11 / v3.0.0
```

2. Add DEPS of the plugin in the Makefile
```
v2.3.11 
DEPS += emqttd_kafka_bridge
dep_emqttd_kafka_bridge = git https://github.com/guide16/emqx_kafka_bridge.git master

v3.0.0
DEPS += emqx_kafka_bridge
dep_emqttd_kafka_bridge = git https://github.com/guide16/emqx_kafka_bridge.git release

3. Add load plugin in relx.config

```
v2.3.11 
{emqttd_kafka_bridge, load},

v3.0.0
{emqx_kafka_bridge, load},

```

4. Build
```
cd emq-relx && make
```  
Configuration
----------------------
You will have to edit the configurations of the bridge to set the kafka Ip address and port.

Edit the file emq-relx/deps/emqttd_kafka_bridge/etc/emqttd_kafka_bridge.config
```
v2.3.11 
[
  {emqttd_kafka_bridge, [{values, [
      {bootstrap_broker, [{"172.19.16.67",19092},{"172.19.16.68",19092},{"172.19.16.69",19092}]},
      {client_config, [
        {auto_start_producers, true},
        {allow_topic_auto_creation, true},
        {query_api_versions,false},
        {reconnect_cool_down_seconds, 10}
      ]},
      {kafka_producer_topic, [
        {"device", <<"saas_device_downstream">>},
        {"client", <<"saas_client_downstream">>},
        {"paas", <<"paas_sqdata_upstream">>}
      ]},
      {kafka_producer_partitions, 10}
    ]}]}
].

v3.0.0 
emqx.kafka.bridge.broker = 172.19.16.67:19092, 172.19.16.68:19092, 172.19.16.69:19092
emqx.kafka.bridge.partition = 10
emqx.kafka.bridge.client.flag = auto_start_producers:true, allow_topic_auto_creation:false, query_api_versions:false
emqx.kafka.bridge.client.integer = reconnect_cool_down_seconds:10
emqx.kafka.bridge.regex = ^(client|device|paas)/products/(\\S+)/devices/(\\S+)/(command)(/\\S+)*$
emqx.kafka.bridge.topic = device:saas_device_downstream, client:saas_client_downstream, paas: paas_sqdata_upstream

```

Start the EMQ broker and load the plugin 
-----------------
v2.3.11 
1) cd emq-relx/_rel/emqttd
2) ./bin/emqttd start
3) ./bin/emqttd_ctl plugins load emqttd_kafka_bridge

v3.0.0 
1) cd emq-relx/_rel/emqx
2) ./bin/emqx start
3) ./bin/emqx_ctl plugins load emqx_kafka_bridge
