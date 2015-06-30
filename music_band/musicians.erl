-module(musicians).
-behavior(gen_server).

-export([start_link/1, stop/1]).

-record(state, {name="", role, skill=good}).
-define(DELAY, 750).

start_link(Role, Skill) ->
	gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
	gen_server:call(Role, stop).