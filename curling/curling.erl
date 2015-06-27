-module(curling).
-export([start_link/2, set_teams/3, add_points/3, next_round/1, join_feed/2, leave_feed/2]).

start_link(TeamA, TeamB) ->
	{ok, Pid} = gen_event:start_link(),
	gen_event:add_handler(Pid, curling_scoreboard, []),
	set_teams(Pid, TeamA, TeamB),
	{ok, Pid}.

set_teams(Pid, TeamA, TeamB) ->
	gen_event:notify(Pid, {set_team, TeamA, TeamB}).

add_points(Pid, Team, N) ->
	gen_event:notify(Pid, {add_points, Team, N}).

next_round(Pid) ->
	gen_event:notify(Pid,  next_round).

%% Subscribe the pid to the feed event
join_feed(Pid, ToPid) ->
	HandlerId = {curling_feed, make_ref()},
	gen_event:add_handler(Pid, HandlerId, [ToPid]),
	HandlerId.

%% Unsubscribe
leave_feed(Pid, HandlerId) ->
	gen_event:delete_handler(Pid, HandlerId, leave_feed).