-module(curling_scoreboard_hw).

-export([add_point/1, next_round/0, set_teams/2, reset_board/0]).

%% THIS IS A MOCK MODULE

set_teams(TeamA, TeamB) ->
	io:format("Scoreboard: Team ~s vs Team ~s~n", [TeamA, TeamB]).

next_round() ->
	io:format("Scoreboard: round is over").

add_point(Team) ->
	io:format("Scoreboard: increased point to team ~s", [Team]).

reset_board() ->
	io:format("Scoreboard: score and teams has been reset").