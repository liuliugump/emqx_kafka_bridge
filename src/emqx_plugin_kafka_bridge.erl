%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_plugin_kafka_bridge).

-include_lib("emqx/include/emqx.hrl").

-export([ load/1
        , unload/0
        ]).

-define(APP, emqx_plugin_kafka_bridge).


%% Client Lifecircle Hooks
-export([ on_client_connected/3
        , on_client_disconnected/4
        , on_client_subscribe/4
        , on_client_unsubscribe/4
        ]).

%% Session Lifecircle Hooks
-export([ on_session_created/3
        , on_session_subscribed/4
        , on_session_unsubscribed/4
        , on_session_resumed/3
        , on_session_discarded/3
        , on_session_takeovered/3
        , on_session_terminated/4
        ]).

%% Message Pubsub Hooks
-export([ on_message_publish/2
        , on_message_delivered/3
        , on_message_acked/3
        , on_message_dropped/4
        ]).

%% Called when the plugin application start
load(Env) ->
    brod_init([Env]),
    emqx:hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
    emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
    emqx:hook('client.subscribe',    {?MODULE, on_client_subscribe, [Env]}),
    emqx:hook('client.unsubscribe',  {?MODULE, on_client_unsubscribe, [Env]}),
    emqx:hook('session.created',     {?MODULE, on_session_created, [Env]}),
    emqx:hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
    emqx:hook('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
    emqx:hook('session.resumed',     {?MODULE, on_session_resumed, [Env]}),
    emqx:hook('session.discarded',   {?MODULE, on_session_discarded, [Env]}),
    emqx:hook('session.takeovered',  {?MODULE, on_session_takeovered, [Env]}),
    emqx:hook('session.terminated',  {?MODULE, on_session_terminated, [Env]}),
    emqx:hook('message.publish',     {?MODULE, on_message_publish, [Env]}),
    emqx:hook('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
    emqx:hook('message.acked',       {?MODULE, on_message_acked, [Env]}),
    emqx:hook('message.dropped',     {?MODULE, on_message_dropped, [Env]}).

%%--------------------------------------------------------------------
%% Client Lifecircle Hooks
%%--------------------------------------------------------------------



on_client_connected(ClientInfo = #{clientid := ClientId, username := Username}, ConnInfo, _Env) ->
    io:format("Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
            [ClientId, ClientInfo, ConnInfo]).
    {ok, Props}.   

on_client_disconnected(ClientInfo = #{clientid := ClientId, username := Username}, ReasonCode, ConnInfo, _Env) ->
    io:format("Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ReasonCode, ClientInfo, ConnInfo]),
    % Now = erlang:timestamp(),
    % Action = <<"disconnected">>,
    % Payload = [{client_id, ClientId}, {action, Action}, {username, Username}, {reason, ReasonCode}, {ts, emqx_time:now_secs(Now)}],
    % Disconnected = proplists:get_value(disconnected, _Env),
    % produce_kafka_payload(Disconnected, Username, Payload, _Env),
    {ok, Props}.



on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    io:format("Client(~s) will subscribe: ~p~n", [ClientId, TopicFilters]),
    {ok, TopicFilters}.

on_client_unsubscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    io:format("Client(~s) will unsubscribe ~p~n", [ClientId, TopicFilters]),
    {ok, TopicFilters}.

%%--------------------------------------------------------------------
%% Session Lifecircle Hooks
%%--------------------------------------------------------------------

on_session_created(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) created, Session Info:~n~p~n", [ClientId, SessInfo]).
    % Now = erlang:timestamp(),
    % Action = <<"connected">>,
    % Username = proplists:get_value(username, SessInfo),
    % Payload = [{client_id, ClientId}, {username, Username}, {action, Action},  {ts, emqx_time:now_secs(Now)}],
    % Connected = proplists:get_value(connected, _Env),
    % produce_kafka_payload(Connected, Username, Payload, _Env).

on_session_subscribed(#{clientid := ClientId, username := Username}, Topic, SubOpts, _Env) ->
    io:format("Session(~s) subscribed ~s with subopts: ~p~n", [ClientId, Topic, SubOpts]).
    % Now = erlang:timestamp(),
    % Action = <<"subscribed">>,
    % Payload = [{client_id, ClientId}, {action, Action}, {username, Username}, {topic, Topic}, {ts, emqx_time:now_secs(Now)}],
    % Subscribed = proplists:get_value(subscribed, _Env),
    % produce_kafka_payload(Subscribed, Username, Payload, _Env).

on_session_unsubscribed(#{clientid := ClientId}, Topic, Opts, _Env) ->
    io:format("Session(~s) unsubscribed ~s with opts: ~p~n", [ClientId, Topic, Opts]).

on_session_resumed(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) resumed, Session Info:~n~p~n", [ClientId, SessInfo]).
    ok.

on_session_discarded(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is discarded. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_takeovered(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is takeovered. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_terminated(_ClientInfo = #{clientid := ClientId}, Reason, SessInfo, _Env) ->
    io:format("Session(~s) is terminated due to ~p~nSession Info: ~p~n",
              [ClientId, Reason, SessInfo]).

%%--------------------------------------------------------------------
%% Message PubSub Hooks
%%--------------------------------------------------------------------

%% Transform message and return
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

on_message_publish(Message = #message{id = MsgId,
                        qos = Qos,
                        from = From,
                        flags = Flags,
                        topic  = Topic,
                        payload = Payload,
                        timestamp  = Time}, _Env) ->
    io:format("Publish ~s~n", [emqx_message:format(Message)]),

    MP =  proplists:get_value(regex, _Env),
    Username = emqx_message:get_header(username,Message),
    case re:run(Topic, MP, [{capture, all_but_first, list}]) of
       nomatch -> {ok, Message};
       {match, Captured} -> [Type, ProductId, DevKey|Fix] = Captured,
         Topics = proplists:get_value(topic, _Env),
         case proplists:get_value(Type, Topics) of
             undefined -> io:format("publish no match topic ~s", [Type]);            
             ProduceTopic -> 
                  Key = iolist_to_binary([ProductId,"_",DevKey,"_",Fix]),
                  Partition = proplists:get_value(partition, _Env),
                  Now = erlang:timestamp(),
                  Msg = [{client_id, From}, {action, <<"message_publish">>},{topic,Topic},{username,Username}, {payload, Payload}, {ts, emqx_time:now_secs(Now)}],
                  {ok, MessageBody} = emqx_json:safe_encode(Msg),
                  MsgPayload = iolist_to_binary(MessageBody),
                  ok = brod:produce_sync(brod_client_1, ProduceTopic, getPartiton(Key,Partition), Username, MsgPayload)
        end,
       {ok, Message}
    end.

on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason, _Env) ->
    ok;
on_message_dropped(Message = #message{}, _By = #{node := Node}, Reason, _Env) ->
    io:format("Message dropped by node ~s due to ~s: ~s~n",
              [Node, Reason, emqx_message:format(Message)]).

on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    io:format("Message delivered to client(~s): ~s~n",
              [ClientId, emqx_message:format(Message)]),
    {ok, Message}.

on_message_acked(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    io:format("Message acked by client(~s): ~s~n",
              [ClientId, emqx_message:format(Message)]).

brod_init(_Env) ->
    {ok, _} = application:ensure_all_started(brod),
    {ok, BootstrapBroker} = application:get_env(?APP, broker),
    {ok, ClientConfig} = application:get_env(?APP, client),
    ok = brod:start_client(BootstrapBroker, brod_client_1, ClientConfig),
    io:format("Init EMQX-Kafka-Bridge with ~p~n", [BootstrapBroker]).

getPartiton(Key, Partitions) ->
     <<Fix:120, Match:8>> = crypto:hash(md5, Key),
     abs(Match) rem Partitions.





%% Called when the plugin application stop
unload() ->
    emqx:unhook('client.connected',    {?MODULE, on_client_connected}),
    emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}),
    emqx:unhook('client.subscribe',    {?MODULE, on_client_subscribe}),
    emqx:unhook('client.unsubscribe',  {?MODULE, on_client_unsubscribe}),
    emqx:unhook('session.created',     {?MODULE, on_session_created}),
    emqx:unhook('session.subscribed',  {?MODULE, on_session_subscribed}),
    emqx:unhook('session.unsubscribed',{?MODULE, on_session_unsubscribed}),
    emqx:unhook('session.resumed',     {?MODULE, on_session_resumed}),
    emqx:unhook('session.discarded',   {?MODULE, on_session_discarded}),
    emqx:unhook('session.takeovered',  {?MODULE, on_session_takeovered}),
    emqx:unhook('session.terminated',  {?MODULE, on_session_terminated}),
    emqx:unhook('message.publish',     {?MODULE, on_message_publish}),
    emqx:unhook('message.delivered',   {?MODULE, on_message_delivered}),
    emqx:unhook('message.acked',       {?MODULE, on_message_acked}),
    emqx:unhook('message.dropped',     {?MODULE, on_message_dropped}).

produce_kafka_payload(Key, Username, Message, _Env) ->
    {ok, MessageBody} = emqx_json:safe_encode(Message),
    % MessageBody64 = base64:encode_to_string(MessageBody),
    Payload = iolist_to_binary(MessageBody),
    Partition = proplists:get_value(partition, _Env),
    Topic = iolist_to_binary(Key),
    brod:produce_sync(brod_client_1, Topic, getPartiton(Username,Partition), Username, Payload).