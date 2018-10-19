Build the EMQ broker
-------------

1. clone emq-relx project
```	
git clone https://github.com/emqtt/emq-relx.git
```
2. Add DEPS of the plugin in the Makefile
```
DEPS += emqttd_kafka_bridge
dep_emqttd_kafka_bridge = git https://xxxxxxxxxxx.git master
```
3. Add load plugin in relx.config
```
{emqttd_kafka_bridge, load},
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
[
  {emqttd_kafka_bridge, [{values, [
      {bootstrap_broker, [{"172.19.3.186", 9092}] },
      {query_api_versions,false},
      {reconnect_cool_down_seconds, 10},
      {kafka_producer_partitions, 1}
    ]}]}
]
```

Start the EMQ broker and load the plugin 
-----------------
1) cd emq-relx/_rel/emqttd
2) ./bin/emqttd start
3) ./bin/emqttd_ctl plugins load emqttd_kafka_bridge




