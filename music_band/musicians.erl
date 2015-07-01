-module(musicians).
-behavior(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {name="", role, skill=good}).
-define(DELAY, 750).

start_link(Role, Skill) ->
	gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
	gen_server:call(Role, stop).

init([Role, Skill]) ->
	%% to know when the parent shuts down
	process_flag(trap_exit, true),
	%% Set a seed for a random number generation
	random:seed(now()),
	TimeToPlay = random:uniform(3000),
	Name = pick_name(),
	StrRole = atom_to_list(Role),
	io_format("Musician ~s, playing the ~s entered the room", [Name, StrRole]),
	{ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.

pick_name() ->
	lists:nth(random:uniform(10), firstnames())
	++ " " ++
	lists:nth(random:uniform(10), lastnames()).

firstnames() ->
	["Manuela", "Candela", "Martin", "Martinh0", "Marco", "Miguel", "Marián", "Jueimei", "Jero", "Adrian"].

lastnames() ->
	["Vintenove", "Loira", "Gomez", "Otero", "Ma", "Hsu", "Trevijano", "Raskolnikov", "Oblomov", "Russell"].

handle_call(stop, _From, S=#state{}) ->
	{stop, normal, ok, S};

handle_call(_Message, _From, S) ->
	{noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
	{noreply, S, ?DELAY}.
