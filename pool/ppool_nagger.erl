-module(ppool_nagger).
-behavior(gen_server).

-export([start_link/4, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link(Task, Delay, Max, SendTo) ->
	gen_server:start_link(?MODULE, {Task, Delay, Max, SendTo}, []).

stop(Pid) ->
	gen_server:call(Pid, stop).

init({Task, Delay, Max, SentTo}) ->
	{ok, {Task, Delay, Max, SentTo}, Delay}.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(_Message, _From, State) ->
	{noreply, State}.

handle_cast(_Message, State) ->
	{noreply, State}.

handle_info(timeout, {Task, Delay, Max, SentTo}) ->
	SentTo ! {self(), Task},
	if Max =:= infinity ->
		{noreply, {Task, Delay, Max, SentTo}, Delay};
	   Max =< 1 ->
	    {stop, normal, ok, {Task, Delay, 0, SentTo}};
	    Max > 1 ->
	    {noreply, {Task, Delay, Max-1, SentTo}, Delay}
	end.

code_change(_OldSvn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) -> ok.