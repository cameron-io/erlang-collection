%%%-------------------------------------------------------------------
%%% @doc resource_manager Public API
%%% @author Cameron G.
%%% @copyright (C) 2021
%%% Created : 17. Aug 2021 00:17
%%%-------------------------------------------------------------------
-module(resource_manager).
-author("Cameron Gallichan").

-behavior(application).

%% API
-export([start/2, stop/1, reserve/0, unreserve/1]).

-type resource()::any().

-record(
	state,
	{													% Example
		free :: [resource()],							%		free = [1,2,3]
		reserved = [] :: [{resource(), pid()}],			%		reserved = [{1, <0.89.0>}, {3, <0.96.0>}, {...}]
		monitors = [] :: [{resource(), monitor}]		% 	monitors = [{1, #Ref<0.xxx.xxx.xx>, {...}]
	}
).

%% Entry Point

-spec start(_, [resource()]) -> ok | {error, Reason::any()}.
start(_StartType, Resources) when erlang:is_list(Resources) ->
	Pid = erlang:spawn(fun() -> worker(Resources) end),
	erlang:register(?MODULE, Pid),
	{ok, Pid}.

stop(_State) ->
	ok.

%% Worker Process Loop

worker(Resources) ->
	State = #state{free = Resources},
	loop(State).

loop(State) ->
	receive
		{Pid, reserve} ->
			{NewState, Reply} = reserve(State, Pid),
			Pid ! Reply,
			loop(NewState);
		{Pid, {unreserve, Resource}} ->
			{NewState, Reply} = unreserve(State, {Resource, Pid}),
			Pid ! Reply,
			loop(NewState);
		{'DOWN', Ref, process, Pid, _Reason} ->
			{NewState, Reply} = refresh_state(State, Ref),
			Pid ! Reply,
			loop(NewState)
	end.

%% Process Messaging

-spec reserve() -> {ok, resource()} | {error, none_available}.
reserve() ->
	send(reserve).

-spec unreserve(resource()) -> ok | {error, Reason::any()}.
unreserve(Resource) ->
	send({unreserve, Resource}).

send(Message) ->
	?MODULE ! {erlang:self(), Message},
	receive
		Reply ->
			Reply
	end.

%% Resource Management

reserve(#state{free = []} = State, _) -> {State, none_available};
reserve(#state{free = [R|Rs], reserved = ReservedItemsList, monitors = MonitorsList} = State, Pid) ->
	MonitorRef = erlang:monitor(process, Pid),
	{
		State#state{
			free = Rs,
			reserved = [{R, Pid} | ReservedItemsList],
			monitors = [{R, MonitorRef} | MonitorsList]
		},
		{ok, R}
	}.

unreserve(#state{reserved = []} = State, _) -> {State, {error, not_reserved}};
unreserve(#state{free = Free, reserved = ReservedItemsList, monitors = MonitorsList} = State, {Resource, _Pid} = ReservedItem) ->
	case lists:keytake(Resource, 1, ReservedItemsList) of
		{value, ReservedItem, NewReservedItemsTuple} ->
			{
				ok,
      		 	State#state{
					free = [Resource | Free],
					reserved = NewReservedItemsTuple,
					monitors = end_monitor(Resource, MonitorsList)
				}
			};
		{value, _, _} ->
			{State, {error, not_your_resource}};
		false ->
			{State, {error, unable_to_modify_state}}
	end.

%% State Management

refresh_state(#state{free = FreeItemsList, reserved = ReservedItemsList, monitors = MonitorsList} = State, Ref) ->
	case lists:keytake(Ref, 2, MonitorsList) of
		{value, {Resource, _Ref}, NewMonitors} ->
			{
        		ok,
				State#state{
					free = [Resource|FreeItemsList],
					reserved = lists:keydelete(Resource, 1, ReservedItemsList),
					monitors = NewMonitors
				}
			};
		false ->
			{State, {error, unable_to_modify_state}}
	end.

%% Monitor Management

end_monitor(Resource, Monitors) ->
	{_, {_, MonitorRef}, NewMonitors} = lists:keytake(Resource, 1, Monitors),
	erlang:demonitor(MonitorRef),
	NewMonitors.
