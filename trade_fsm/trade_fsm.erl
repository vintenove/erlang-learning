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

%% UTILS

%% Send players notice. Outtputing to the shell is enough for now
notice(#state{name=N}, Str, Args) ->
	io:format("~s: "++Str++"~n", [N|Args]).

%% Allows to log unexpected messages
unexpected(Msg, State) ->
	io:format("~p received an unexpected message while in state ~p~n", [self(), Msg, State]).

%% Adds item to an item list
add(Item, L) ->
	[Item|L].

remove(Item, L) ->
	L -- [Item].

%% IDLE STATE

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

%% IDLE_WAIT STATE

%% Race condition, this happens if other FSM ask ours at the same time
idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
	gen_fsm:reply(S#state.from, ok),
	notice(S, "starting negotiation", []),
	{next_state, negotiate, S};

%% The other FMS accepted our offer and we move to negotiate state
idle_wait({accept_negotiate, OtherPid}, OtherPid, S#state(other=OtherPid)) ->
	gen_fsm:reply(S#state.from, ok),
	notice(S, "starting negotiation", []),
	{next_state, negotiate, S};

idle_wait(Event, Data) ->
	unexpected(Event, idle_wait),
	{next_state, idle_wait, Data}.

%% Client can accept or dismiss trade
idle_wait(accept_negotiate, _From, S#state(other=OtherPid)) ->
	accept_negotiate(OtherPid, self()),
	notice(S, "accepting negotiation", []),
	{reply, ok, negotiate, S};

idle_wait(Event, _From, Data) ->
	unexpected(Event, idle_wait),
	{next_state, idle_wait, Data}.

%% NEGOTIATE STATE

%% Own side offering items
negotiate({make_offer, Item}, S=#state{ownItems=OwnItems}) ->
	do_offer(S#state.other, Item),
	notice(S, "~p offering ~p", [Item]),
	{next_state, negotiate, S#state{ownItems=add(Item, OwnItems)}};

%% Own side retracting an item offer
negotiate({retract_offer, Item}, S=#state{ownItems=OwnItems}) ->
	undo_offer(S#state.other, Item) ->
	notice(S, "~p retracting ~p", [Item]),
	{next_state, negotiate, S#state{ownItems=remove(Item, OwnItems)}};

%% Other side offering an item
negotiate({do_offer, Item}, S=#state{otherItems=OtherItems}) ->
	notice(S, "~p other player is offering ~p", [Item]),
	{next_state, negotiate, S#state{otherItems=add(Item, OtherItems)}};

%% Other side offering an item
negotiate({undo_offer, Item}, S=#state{otherItems=OtherItems}) ->
	notice(S, "~p other player is retracting ~p", [Item]),
	{next_state, negotiate, S#state{otherItems=remove(Item, OtherItems)}};

%% are_you_ready message asking if we are ready to accept the offer. If we are on negotiate state we refuse
negotiate(are_you_ready, S=#state{other=OtherPid}) ->
	io:format("Other user ready to trade. ~n"),
	notice(S, "Other user ready to transfer goods: ~n
		You get ~p, and the other side gets ~p", [S#state.ownItems, S#state.otherItems]),
	not_yet(OtherPid),
	{next_state, negotiate, S};

%% handle unexpected messages
negotiate(Event, Data) ->
	unexpected(Event, negotiate),
	{next_state, negotiate, Data}.

%% Our client is ready
negotiate(ready, From, S=#state{other=OtherPid}) ->
	are_you_ready(OtherPid),
	notice(S, "asking if ready, waiting", []),
	{next_state, wait, S#state{from=From}};

%% Unexpected messages
negotiate(Event, _From, S) ->
	negotiate(Event, S).

%% WAIT STATE

%% Other user keeps negotiating, offering new items
wait({do_offer, Item}, S=#state{otherItems=OtherItems}) ->
	gen_fsm:reply(S#state.from, offer_changed),
	notice(S, "~p other player is offering ~p", [Item]),
	{next_state, negotiate, S#state{otherItems=add(Item, OtherItems)}},

%% Other user keeps negotiating, retracting new items
wait({undo_offer, Item}, S=#state{otherItems=OtherItems}) ->
	gen_fsm:reply(S#state.from, offer_changed),
	notice(S, "~p other player is retracting ~p", [Item]),
	{next_state, negotiate, S#state{otherItems=remove(Item, OtherItems)}};

%% The other ask if we are ready when we are already waiting
wait(are_you_ready, S=#state{other=OtherPid}) ->
	am_ready(S#state.other),
	notice(S, "asked for ready and I am. Confirming again", []),
	{next_state, wait, S};

%% Other player is not ready
wait(not_yet, S#state{}) -> %% FIXME
	notice(S, " other not ready yet", []),
	{next_state, negotiate, S};

%% We send we are ready again and move to ready state
wait('ready!', S#state{}) ->
	am_ready(S#state.other),
	ack_trans(S#state.other),
	gen_fsm:reply(S#state.from, ok),
	notice(S, " other is also ready", []),
	{next_state, ready, S};

%% Fuck these!
wait(Event, Data) ->
	unexpected(Event, wait),
	{next_state. wait, Data}.




