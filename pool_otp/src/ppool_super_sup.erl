-module(ppool_super_sup).
-behavior(supervisor).

-export([start_link/0, init/1, start_pool/3, stop_pool/1]).

start_link() ->
	supervisor:start_link({local, ppool}, ?MODULE, []).

init([]) ->
	MaxRestart = 6,
	MaxTime = 3600,
	{ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

start_pool(Name, Limit, MFA) ->
	ChildSpec = {Name, {ppool_sup, start_link, [Name, Limit, MFA]},
	permanent, 10500, supervisor, [ppool_sup]},
	supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
	supervisor:terminate_child(ppool_sup, Name),
	supervisor:delete_child(ppool_sup, Name).

