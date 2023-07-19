-module(event_driven).

-behaviour(application).

-export([start/2, start_subscriber/1, publish_event/0, publish_event/2, stop/1]).

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
start_subscriber(Name) ->
    Pid = erlang:spawn(fun() -> init_subscriber_actor(Name) end),
    PName = to_subscriber_name(Name),
    true = erlang:register(PName, Pid),
    ok = send(?EXCHANGE, {self(), {new_subscriber, {name, PName}}}),
    {ok, Pid}.

publish_event() ->
    %% Publish Purchase Event to Exchange 
    ok = publish_event(purchase_complete, [{quantity, 1}, {item, "Shirt"}]).

publish_event(Type, Message) ->
    ok = send(?EXCHANGE, {self(), {Type, Message}}).

%% Internal Functions

send(PName, Message) ->
    PName ! Message,
    receive
        Reply ->
            Reply
    after 1000 ->
        io:format("Process Messaging timeout: ~p", [{PName, Message}])
    end.

to_subscriber_name(EventType) ->
    binary_to_atom(<<"exchange_subscriber_", (atom_to_binary(EventType))/binary>>).

init_subscriber_actor(EventType) ->
    InitState = #{event_type => EventType},
    subscriber_actor(InitState).

subscriber_actor(State) ->
    receive
        {Pid, {purchase_complete, _Message}} ->
            Pid ! ok,
            subscriber_actor(State);
        {_Pid, stop} ->
            ok
    end.

init_exchange_actor() ->
    InitState = #{subscribers => []},
    exchange_actor(InitState).

exchange_actor(#{subscribers := Subscribers} = State) ->
    receive
        {Pid, {purchase_complete = _EventType, _EventMessage} = Event} ->
            %% Notify the subscribers
            [send(PName, Event) || {PName, _} <- Subscribers],
            Pid ! ok,
            exchange_actor(State);
        {Pid, {new_subscriber, {name, PName}}} ->
            NewState = State#{subscribers => [PName|Subscribers]},
            Pid ! ok,
            exchange_actor(NewState);
        {_Pid, stop} ->
            ok;
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            exchange_actor(State)
    end.


