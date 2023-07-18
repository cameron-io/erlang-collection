%%%-------------------------------------------------------------------
%%% @doc resource_manager Public API
%%% @author Cameron G.
%%% @copyright (C) 2023
%%%-------------------------------------------------------------------
-module(resource_manager).
-author("Cameron Gallichan").

-behavior(application).

%% API
-export([start/2, stop/1, reserve/0, unreserve/1]).

-type resource()::any().

-type state() :: #{
    free => [resource()],
    reserved => [{resource(), pid()}],
    monitors => [{resource(), pid()}]
}.

%% API Functions

-spec start(_, [resource()]) ->
    ok | {error, Reason::any()}.
start(_StartType, Resources) when erlang:is_list(Resources) ->
    Pid = erlang:spawn(fun() -> init_actor(Resources) end),
    erlang:register(?MODULE, Pid),
    {ok, Pid}.

stop(_State) ->
	ok.

-spec reserve() ->
    {ok, resource()} | {error, none_available}.
reserve() ->
    send(reserve).

-spec unreserve(resource()) ->
    ok | {error, Reason::any()}.
unreserve(Resource) ->
    send({unreserve, Resource}).


%% Mail Box

send(Message) ->
    ?MODULE ! {erlang:self(), Message},
    receive
        Reply ->
            Reply
    end.


%% Actor Process Loop

init_actor(Resources) ->
    InitState = #{
        free => Resources,
        reserved => [],
        monitors => []
    },
    actor_loop(InitState).

actor_loop(State) ->
	receive
		{Pid, reserve} ->
			{NewState, Reply} = reserve(State, Pid),
			Pid ! Reply,
			actor_loop(NewState);
		{Pid, {unreserve, Resource}} ->
			{NewState, Reply} = unreserve(State, {Resource, Pid}),
			Pid ! Reply,
			actor_loop(NewState);
		{'DOWN', Ref, process, Pid, _Reason} ->
			{NewState, Reply} = refresh_state(State, Ref),
			Pid ! Reply,
			actor_loop(NewState)
	end.


%% Message Handlers

-spec reserve(state(), pid()) ->
    {state(), reply}.

reserve(#{free := []} = State, _) ->
    {State, none_available};

reserve(
    #{
        free := [R|Rs],
        reserved := ReservedItemsList,
        monitors := MonitorsList
    } = State,
    Pid
) ->
    MonitorRef = erlang:monitor(process, Pid),
    Reply = {ok, R},
    {State#{
        free => Rs,
        reserved => [{R, Pid} | ReservedItemsList],
        monitors => [{R, MonitorRef} | MonitorsList]
    }, Reply}.

unreserve(#{reserved := []} = State, _) ->
    {State, {error, nothing_reserved}};

unreserve(
    #{
        free := Free,
        reserved := ReservedItemsList,
        monitors := MonitorsList
    } = State,
    {Resource, _Pid} = ReservedItem
) ->
	case lists:keytake(Resource, 1, ReservedItemsList) of
		{value, ReservedItem, NewReservedItemsTuple} ->
			{State#{
                free => [Resource | Free],
                reserved => NewReservedItemsTuple,
                monitors => demonitor_resource(Resource, MonitorsList)
            }, _Reply = ok};
        {value, _, _} ->
            {State, {error, resource_from_different_pid}};
        false ->
            {State, {error, resource_not_reserved}}
    end.

refresh_state(
    #{
        free := FreeItemsList,
        reserved := ReservedItemsList,
        monitors := MonitorsList
    } = State,
    Ref
) ->
    case lists:keytake(Ref, 2, MonitorsList) of
        {value, {Resource, _Ref}, NewMonitors} ->
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


