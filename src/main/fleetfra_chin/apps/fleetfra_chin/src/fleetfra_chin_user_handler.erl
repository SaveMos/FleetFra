%%%-------------------------------------------------------------------
%%% @author Saverio
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. feb 2025 10:18
%%%-------------------------------------------------------------------
-module(fleetfra_chin_user_handler).
-author("Saverio").
-behaviour(gen_server).

%% API
-export([start_link/2, send_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Starts a user handler process and registers it under a unique name.
start_link(GameID, PlayerID) ->
  Name = utility:concat_game_player(GameID, PlayerID),
  gen_server:start_link({local, Name}, ?MODULE, [GameID, PlayerID], []).

%% @doc Sends a message to the user handler process, which forwards it to the WebSocket client.
send_message(Name, Msg) ->
  gen_server:cast(Name, {send, Msg}).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([GameID, PlayerID]) ->
  Name = utility:concat_game_player(GameID, PlayerID),
  case erlang:whereis(Name) of
    undefined ->
      pass;
    _ ->
      erlang:unregister(Name)
  end,
  erlang:register(Name, erlang:self()),
  {ok, #{ game_id => GameID, player_id => PlayerID }}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({send, Msg}, State) ->
  io:format("USER-HANDLER: Player2 received game update: ~p~n", [Msg]),
  %% Forward message to the WebSocket handler

  JsonMessage = jsx:encode([{<<"type">>, game_update}, {<<"msg">>, Msg}]),
  {{text, JsonMessage}, Msg}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.