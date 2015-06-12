-module(trade_fsm).
-behavior(gen_fsm).

%% Public API
-export([start/1, start_link/1, trade/2, accept_trade/1,
 make_offer/2, retract_offer/2, ready/1, cancel/1]).

 %% gen_fsm callbacks
 -export([init/1, handle_event/3, handle_sync_event/4,
 	handle_info/3, terminate/3, code_change/4,
 	idle/2, idle/3, idle_wait/3, negotiate/2, negotiate/3, wait/2,
 	ready/2, ready/3]).

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