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
    %% Spawn Publisher Actor along with its Subscribed Actors
    Pid = erlang:spawn(fun() -> init_publisher_actor() end),
    erlang:register(event_publisher, Pid),
    {ok, EmailSubPid} = start_subscriber(email),
    {ok, InventorySubPid} = start_subscriber(inventory),
    {ok, ShippingSubPid} = start_subscriber(shipping),
    %% Start Subscribers
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

init_publisher_actor() ->
    InitState = #{},
    publisher_actor(InitState).

publisher_actor(State) ->
    receive
        {_Pid, {purchase_complete = EventType, EventMessage}} ->
            %% Process the event, update state, change behaviour etc.
            ok = publish_event(EventType, EventMessage),
            publisher_actor(State);
        {_Pid, exit} ->
            ok;
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            %% Remove subscribed actor
            publisher_actor(State)
    end.

publish_event(_Type, Message) ->
    Message.


