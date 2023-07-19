-module(event_driven).

-behaviour(application).

-export([start/2, start_subscriber/1, stop/1]).

-define(EXCHANGE, event_exchange).

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
    true = erlang:register(?EXCHANGE, Pid),
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
start_subscriber(EventType) ->
    Pid = erlang:spawn(fun() -> init_subscriber_actor(EventType) end),
    true = erlang:register(EventType, Pid),
    {ok, Pid}.


%% Internal Functions

send(PName, Message) ->
    PName ! Message,
    receive
        Reply ->
            Reply
    after 1000 ->
        io:format("Process Messaging timeout: ~p", [{PName, Message}])
    end.

init_subscriber_actor(EventType) ->
    InitState = #{event_type => EventType},
    subscriber_actor(InitState).

subscriber_actor(State) ->
    receive
        {_Pid, {purchase_complete, _Message}} ->
            subscriber_actor(State);
        {_Pid, stop} ->
            ok
    end.

init_exchange_actor() ->
    InitState = #{subscribers => []},
    exchange_actor(InitState).

exchange_actor(State) ->
    receive
        {_Pid, {purchase_complete = _EventType, _EventMessage} = Event} ->
            %% Notify the subscribers
            #{subscribers := Subscribers} = State,
            [send(PName, Event) || {PName, _} <- Subscribers],
            exchange_actor(State);
        {_Pid, {new_subscriber, _SubscriberType}} ->
            exchange_actor(State);
        {_Pid, exit} ->
            ok;
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            exchange_actor(State)
    end.

publish_event(Type, Message) ->
    send(?EXCHANGE, [{Type, Message}]).


