-module(ppool).
-export([start_link/0, stop/0, start_pool/3, stop_pool/1, run/2, sync_queue/2, async_queue/2]).

start_link() ->
	ppool_super_sup:start_link().

stop() ->
	ppool_super_sup:stop().

start_pool(Name, Limit, {M,F,A}) ->
	ppool_super_sup:start_pool(Name, Limit, {M,F,A}).

stop_pool(Name) ->
	ppool_super_sup:stop_pool(Name).

run(Name, Args) ->
	ppool_serv:run(Name, Args).

sync_queue(Name, Args) ->
	ppool_serv:sync_queue(Name, Args).

async_queue(Name, Args) ->
	ppool_serv:async_queue(Name, Args).