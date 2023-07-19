-module(event_driven).

-behaviour(application).

-export([start/2, start_subscriber/1, stop/1]).

-type start_type() :: normal | {takeover, node()} | {failover, node()}.

%% Example Event:
%% - E-Commerce site
%% - purchase should trigger:
%%      send email
%%      update inventory
%%      send shipping instruction

%% API Functions

-spec start(start_type(), [{publisher_name, atom()}|_]) ->
    {ok, pid()}.
start(_StartType, _StartArgs) ->
    %% Spawn Exchange Actor
    Pid = erlang:spawn(fun() -> init_exchange_actor() end),
    true = erlang:register(event_publisher, Pid),
    %% Spawn Subscribers
    {ok, EmailSubPid} = start_subscriber(email),
    {ok, InventorySubPid} = start_subscriber(inventory),
    {ok, ShippingSubPid} = start_subscriber(shipping),
    %% Publish Purchase Event to Exchange 
    ok = publish_event(purchase_complete, [{quantity, 1}, {item, "Shirt"}]),
    {ok, [
        {publisher, Pid},
        {email_subscriber, EmailSubPid},
        {inventory_subscriber, InventorySubPid},
        {shipping_subscriber, ShippingSubPid}
    ]}.

stop(_State) ->
    ok.

-spec start_subscriber(atom()) ->
    ok.
start_subscriber(_EventType) ->
    ok.


%% Internal Functions

init_subscriber_actor() ->
    InitState = #{},
    subscriber_actor(InitState).

subscriber_actor(State) ->
    receive
        {Pid, {EventType, Message}} ->
            ok
    end.


%% Publisher will have its own exchange with events

init_exchange_actor() ->
    InitState = #{},
    publisher_actor(InitState).

exchange_actor(State) ->
    receive
        {_Pid, {purchase_complete = EventType, EventMessage}} ->
            %% Notify the subscribers
            exchange_actor(State);
        {_Pid, {add_subscriber, SubscriberType}} ->
            exchange_actor(State);
        {_Pid, exit} ->
            ok;
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            exchange_actor(State)
    end.

publish_event(_Type, Message) ->
    Message.


