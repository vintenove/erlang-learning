-module(band_supervisor).
-behavior(supervisor).

-export([start_link/1, init/1]).

start_link(Type) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Type).

init(lenient) -> init({one_for_one, 3, 60});

init(angry) -> init({rest_for_one, 2, 60});

init(jerk) -> init({one_for_all, 1, 60});

init(jamband) ->
	{ok, {{simple_one_for_one, 3, 60},
	[{jam_musician,
	{musicians, start_link, []},
		temporary, 1000, worker, [musicians]}
		]}};

init({RestartStrategy, MaxRestart, MaxTime}) ->
	{ok, {{RestartStrategy, MaxRestart, MaxTime},
	[{singer, 
		{musicians, start_link, [singer, good]},
		permanent, 1000, worker, [musicians]},
	 {bass,
		{musicians, start_link, [bass, good]},
		temporary, 1000, worker, [musicians]},
	 {drum,
		{musicians, start_link, [drum, good]},
		transient, 1000, worker, [musicians]},
	 {keytar,
		{musicians, start_link, [keytar, good]},
		transient, 1000, worker, [musicians]}
		]}}.
