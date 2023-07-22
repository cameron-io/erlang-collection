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
    {ok, _EmailSubPid} = start_subscriber(<<"email">>),
    {ok, _InventorySubPid} = start_subscriber(<<"inventory">>),
    {ok, _ShippingSubPid} = start_subscriber(<<"shipping">>),
    %% Verify Exchange knows Subscribers
    {ok, [
        exchange_subscriber_shipping,
        exchange_subscriber_inventory,
        exchange_subscriber_email
    ]} = send(?EXCHANGE, {self(), get_subscribers}).

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
    Event = [{quantity, 1}, {item, "Shirt"}],
    {ok, [
        <<"email - handled">>,
        <<"inventory - handled">>,
        <<"shipping - handled">>
    ]} = publish_event(purchase_complete, Event).

publish_event(Type, Message) ->
    send(?EXCHANGE, {self(), {Type, Message}}).

%% Internal Functions

send(PName, Message) ->
    PName ! Message,
    receive
        Reply ->
            Reply
    after 1000 ->
        io:format("Process Messaging timeout: ~p", [{PName, Message}]),
        {error, timeout}
    end.

to_subscriber_name(EventType) ->
    binary_to_atom(<<"exchange_subscriber_", EventType/binary>>).

init_subscriber_actor(EventType) ->
    InitState = #{event_type => EventType},
    subscriber_actor(InitState).

subscriber_actor(#{event_type := EventType} = State) ->
    receive
        {Pid, {purchase_complete, _Message}} ->
            Pid ! {ok, <<EventType/binary, " - handled">>},
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
            Msgs = lists:foldl(
                fun(SubProcName, Acc) ->
                    {ok, Msg} = send(SubProcName, {self(), Event}),
                    [Msg|Acc]
                end,
                [],
                Subscribers),
            Pid ! {ok, Msgs},
            exchange_actor(State);
        {Pid, {new_subscriber, {name, PName}}} ->
            NewState = State#{subscribers => [PName|Subscribers]},
            Pid ! ok,
            exchange_actor(NewState);
        {Pid, get_subscribers} ->
            Pid ! {ok, Subscribers},
            exchange_actor(State);
        {_Pid, stop} ->
            ok;
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            exchange_actor(State)
    end.


