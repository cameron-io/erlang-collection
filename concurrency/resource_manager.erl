%%%-------------------------------------------------------------------
%%% @doc resource_manager Public API
%%% @author Cameron G.
%%% @copyright (C) 2023
%%%-------------------------------------------------------------------
-module(resource_manager).
-author("Cameron Gallichan").

-behavior(application).

%% API
-export([start_pool/1, start/2, stop/1, reserve/0, unreserve/1]).

-type start_type() :: normal | {takeover, node()} | {failover, node()}.

-type resource() :: term().

-type state() :: #{
    free => [resource()],
    reserved => [{resource(), pid()}],
    monitors => [{resource(), reference()}]
}.

%% API Functions

start_pool(N) ->
    start(normal, [{resources, lists:seq(1, N)}]).

-spec start(start_type(), [{resources, resource()}|_]) ->
    ok | {error, Reason :: term()}.
start(_StartType, StartArgs) when erlang:is_list(StartArgs) ->
    Resources = proplists:get_value(resources, StartArgs),
    Pid = erlang:spawn(fun() -> init_supervisor_actor(Resources) end),
    erlang:register(?MODULE, Pid),
    {ok, Pid}.

stop(_State) ->
	ok.

-spec reserve() ->
    {ok, resource()} | {error, none_available}.
reserve() ->
    send(?MODULE, reserve).

-spec unreserve(resource()) ->
    ok | {error, Reason :: term()}.
unreserve(Resource) ->
    send(?MODULE, {unreserve, Resource}).


%% Post Office

send(PName, Message) ->
    PName ! {self(), Message},
    receive
        Reply ->
            Reply
    after 1000 ->
        io:format("Request Timeout")
    end.


%% Worker Actor Process Loop

init_worker_actor(R) ->
    InitState = #{numbers => []},
    Pid = erlang:spawn(fun() -> worker_actor(InitState) end),
    erlang:register(get_worker_name(R), Pid),
    {ok, Pid}.

worker_actor(#{numbers := NumbersList} = State) ->
    receive
        {Pid, NewNum} when is_integer(NewNum) ->
            NewNumbersList = [NewNum|NumbersList],
            NewState = State#{numbers => NewNumbersList},
            Pid ! {ok, NewNumbersList}, 
            worker_actor(NewState);
        {Pid, exit} ->
            Pid ! ok
    end.

get_worker_name(R) ->
    binary_to_atom(<<"worker_", (integer_to_binary(R))/binary>>).

%% Supervisor Actor Process Loop

init_supervisor_actor(Resources) ->
    InitState = #{
        free => Resources,
        reserved => [],
        monitors => []
    },
    supervisor_actor(InitState).

supervisor_actor(State) ->
	receive
		{CallerPid, reserve} ->
			{NewState, Reply} = reserve(State),
			CallerPid ! Reply,
			supervisor_actor(NewState);
		{CallerPid, {unreserve, Resource}} ->
			{NewState, Reply} = unreserve(State, Resource),
			CallerPid ! Reply,
			supervisor_actor(NewState);
		{'DOWN', DownedMonitorRef, process, Pid, _Reason} ->
			{NewState, Reply} = rollback_state(State, DownedMonitorRef),
			Pid ! Reply,
			supervisor_actor(NewState)
	end.


%% Message Handlers

-spec reserve(state()) ->
    {state(), reply}.

reserve(#{free := []} = State) ->
    {State, none_available};

reserve(
    #{
        free := [R|Rs],
        reserved := ReservedItemsList,
        monitors := MonitorsList
    } = State
) ->
    {ok, Pid} = init_worker_actor(R),
    MonitorRef = erlang:monitor(process, Pid),
    Reply = {ok, {R, Pid}},
    {State#{
        free => Rs,
        reserved => [{R, Pid} | ReservedItemsList],
        monitors => [{R, MonitorRef} | MonitorsList]
    }, Reply}.

unreserve(#{reserved := []} = State, _) ->
    {State, {error, nothing_reserved}};

unreserve(
    #{
        free := FreeItemsList,
        reserved := ReservedItemsList,
        monitors := MonitorsList
    } = State,
    Resource
) ->
	case lists:keytake(Resource, 1, ReservedItemsList) of
		{value, {Resource, _Pid}, NewReservedItemsList} ->
            NewMonitorsList = demonitor_resource(Resource, MonitorsList),
            ok = send(get_worker_name(Resource), exit),
			{State#{
                free => [Resource|FreeItemsList],
                reserved => NewReservedItemsList,
                monitors => NewMonitorsList
            }, _Reply = ok};
        false ->
            {State, {error, resource_not_reserved}}
    end.

rollback_state(
    #{
        free := FreeItemsList,
        reserved := ReservedItemsList,
        monitors := MonitorsList
    } = State,
    DownedMonitorRef
) ->
    case lists:keytake(DownedMonitorRef, 2, MonitorsList) of
        {value, {Resource, DownedMonitorRef}, NewMonitors} ->
            ok = send(get_worker_name(Resource), exit),
            {State#{
                free => [Resource|FreeItemsList],
                reserved => lists:keydelete(Resource, 1, ReservedItemsList),
                monitors => NewMonitors
            }, _Reply = ok};
		false ->
			{State, {error, unable_to_modify_state}}
	end.


%% Monitor Management

demonitor_resource(Resource, Monitors) ->
	{_, {_, MonitorRef}, NewMonitors} = lists:keytake(Resource, 1, Monitors),
	erlang:demonitor(MonitorRef),
	NewMonitors.


