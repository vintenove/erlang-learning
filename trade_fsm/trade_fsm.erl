-module(trade_fsm).
-behavior(gen_fsm).

%% Public API
-export([start/1, start_link/1, trade/2, accept_trade/1,
 make_offer/2, retract_offer/2, ready/1, cancel/1]).

 %%% Public API
 start(Name) ->
 	gen_fsm:start(?MODULE, [Name], []).

 start_link(Name) ->
 	gen_fsm:start_link(?MODULE, [Name], []).

 %% Ask for beginning session. Returns when/if the other accepts.
 trade(OwnPid, OtherPid) ->
 	gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 300000).

 %% Accept someone's trade offer.
 accept_trade(OwnPid) ->
 	gen_fsm:sync_send_event(OwnPid, accept_negotiate).

 %% Send an item on the table to be traded.
 make_offer(OwnPid, Item) ->
 	gen_fsm:send_event(OwnPid, {make_offer, Item}).

 %% Cancel trade offer
retract_offer(OwnPid, Item) ->
	gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% Mention that you're ready for a trade. When the other player 
%% also declares they're ready the trade is done.
ready(OwnPid) ->
	gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% Cancel the transaction
cancel(OwnPid) ->
	gen_fsm:sync_send_event(OwnPid, cancel).

%%%%%%%%  FSM TO FSM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Ask the FMS's pid for a trade session
ask_negotiate(OtherPid, OwnPid) ->
	gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%% Forward the client message accepting the transaction
accept_negotiate(OtherPid, OwnPid) ->
	gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

%% Forward a client's offer
do_offer(OtherPid, Item) ->
	gen_fsm:send_event(OtherPid, {do_offer, Item}).

%% Forward a client's cancellation
undo_offer(OtherPid, Item) ->
	gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%% Ask the other side if he's ready to trade
are_you_ready(OtherPid) ->
	gen_fsm:send_event(OtherPid, are_you_ready).

%% Reply that we are not ready yet
not_yet(OtherPid) ->
	gen_fsm:send_event(OtherPid, not_yet).

%% Reply that we are ready
am_ready(OtherPid) ->
	gen_fsm:send_event(OtherPid, 'ready!').

%% Acknowledge that the fsm is in ready state
ack_trans(OtherPid) ->
	gen_fsm:send_event(OtherPid, ack).

%% Ask if ready to commit
ask_commit(OtherPid) ->
	gen_fsm:send_event(OtherPid, ask_commit).

%% Begin synchronous commit
do_commit(OtherPid) ->
	gen_fsm:sync_send_event(OtherPid, do_commit).

%% Notify cancellation to the other FSM
notifify_cancel(OtherPid) ->
	gen_fsm:send_all_state_event(OtherPid, cancel).


%% GEN_FSM CALLBACKS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([init/1, handle_event/3, handle_sync_event/4,
	handle_info/3, terminate/3, code_change/4,
	idle/2, idle/3, idle_wait/3, negotiate/2, negotiate/3, wait/2,
	ready/2, ready/3]).

-record(state, {name="",
    other,
    ownItems=[],
    otherItems=[],
    monitor,
    from}).

init(Name) ->
	{ok, idle, #state{name=Name}}.


%% Send players notice. Outtputing to the shell is enough for now
notice(#state{name=N}, Str, Args) ->
	io:format("~s: "++Str++"~n", [N|Args]).

%% Allows to log unexpected messages
unexpected(Msg, State) ->
	io:format("~p received an unexpected message while in state ~p~n", [self(), Msg, State]).

%% Asynchronous idle callbacks. This callback has to do with the other FSM
%% since the client connects synchronously.
idle({ask_negotiate, OtherPid}, S=#state{}) ->
	Ref = monitor(process, OtherPid),
	notice(S, "~p asked for a trade negotiation", [OtherPid]),
	{next_state, idle_wait, S#state{other=OtherPid, monitor=Ref}};

idle(Event, Data) ->
	unexpected(Event, idle),
	{next_state, idle, data}.

%% idle synchonous callbacks
idle(ask_negotiate, OtherPid}, From, S#state={}) ->
	ask_negotiate(OtherPid, self()),
	notice(S, "asking user ˜ p for a trade", [OtherPid]),
	Ref = monitor(process, OtherPid),
	{next_state, idle_wait, S#state{other=OtherPid, from=From, monitor=Ref}};

idle(Event, _From, Data) ->
	idle(Event, Data).
