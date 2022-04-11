# emqx_kafka_bridge

This is a plugin for the EMQX broker that sends all messages received by the broker to kafka.

   注意：插件release版本目前适配的是emqx V4.1.5

## Build Erlang/OTP：22.3.4.25
-------------

```
#安装编译依赖
yum -y install make gcc gcc-c++ kernel-devel m4 ncurses-devel openssl-devel

#下载源码并解压
wget https://github.com/erlang/otp/releases/download/OTP-22.3.4.25/otp_src_22.3.4.25.tar.gz
tar zxvf otp_src_22.3.4.25.tar.gz

#编译erlang
cd otp_src_22.3.4.25
./configure --prefix=/usr/local/erlang --with-ssl --enable-threads --enable-smp-support --enable-kernel-poll --enable-hipe --without-java
make -j8
make install

#p配置环境变量
echo "export PATH=/usr/local/erlang/bin:\$PATH" >> /etc/profile
source /etc/profile
```

## Build the EMQ broker
-------------

1. clone emq-relx project
```	
git clone -b 4.1.5 https://github.com/emqtt/emq-relx.git
```
2. Add deps of the plugin in the rebar.config
```
    {deps,
    [emqx
    ...
    , emqx_rule_engine
    , emqx_plugin_template
    ,{emqx_plugin_kafka_bridge, {git ,"https://github.com/liuliugump/emqx_kafka_bridge",{branch,"release"}}}
    ]}.
```
3. Add load plugin in  rebar.config
```
{relx,
...
 , {emqx_psk_file, load}
        , {emqx_plugin_kafka_bridge, load}
        ]}
 ```
4. Build
```
cd emq-relx && make
```  
Configuration
----------------------
You will have to edit the configurations of the bridge to set the kafka Ip address and port.

Edit the file emq-relx/deps/emqx_kafka_bridge/etc/emqx_kafka_bridge.config
```
 emqx.kafka.bridge.broker = 10.4.23.16:9092
 emqx.kafka.bridge.partition = 10
 emqx.kafka.bridge.client.flag = auto_start_producers:true, allow_topic_auto_creation:false, query_api_versions:false
 emqx.kafka.bridge.client.integer = reconnect_cool_down_seconds:10
 emqx.kafka.bridge.regex = ^(client|device|paas)/products/(\\S+)/devices/(\\S+)/(command)(/\\S+)*$
 emqx.kafka.bridge.topic = device:saas_device_downstream,client:saas_client_downstream, paas: paas_sqdata_upstream
 emqx.kafka.bridge.hook.client.connected.topic     = mqtt_client_connected
 emqx.kafka.bridge.hook.client.disconnected.topic  = mqtt_client_disconnected
 emqx.kafka.bridge.hook.client.subscribed.topic = mqtt_client_subscribed
```

Start the EMQ broker and load the plugin 
-----------------
1) cd emq-relx/_rel/emqx
2) ./bin/emqx start
3) ./bin/emqx_ctl plugins load emqx_kafka_bridge

Kafka Topic :
----------------------

1.mqtt_client_connected : Produce a message when an client  connect to the broker


2.mqtt_client_disconnected : Produce a message when the client closed/disconnected

3.mqtt_client_subscribed : Produce a message when an  client  subscribed

4.saas_device_downstream : When a client send a message with topic "device/products/astring/devices/astring/command/astring"

5.saas_client_downstream : When a client send a message with topic "client/products/astring/devices/astring/command/astring"

6.paas_sqdata_upstream :  When a client send a message with topic "paas/products/astring/devices/astring/command/astring"