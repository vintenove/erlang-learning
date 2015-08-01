-module(ppool_super_sup).
-behavior(supervisor).

-export([start_link/0, init/1, stop/0, start_pool/3, stop_pool/1]).

start_link() ->
	supervisor:start_link({local, ppool}, ?MODULE, []).


%% Brutally kill supervisor. TODO: read chapter 19
stop() ->
	case whereis(ppool) of
		P when is_pid(P) ->
			exit(P, kill);
		_ -> ok
	end.

init([]]) ->
	MaxRestart = 6,
	MaxTime = 3600,
	{ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

