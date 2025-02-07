%%%-------------------------------------------------------------------
%%% @author Saverio
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. feb 2025 12:16
%%%-------------------------------------------------------------------
-module(utility).
-author("Saverio").

%% API
-export([bin_to_atom/1, to_str/1, concat_game_player/2]).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Cast a binary object into an atom.
%% @end
%%-------------------------------------------------------------------
bin_to_atom(Bin) ->
  case erlang:is_binary(Bin) of
    true -> erlang:binary_to_atom(Bin, utf8);
    false -> Bin
  end.

to_str(Input) when erlang:is_binary(Input) ->
  % Se l'input è un binary, usa erlang:binary_to_list/1 per convertirlo in una lista di caratteri
  binary:bin_to_list(Input);
to_str(Input) when erlang:is_list(Input) ->
  % Se l'input è un binary, usa erlang:binary_to_list/1 per convertirlo in una lista di caratteri
  Input;
to_str(Input) when erlang:is_atom(Input) ->
  % Se l'input è un atomo, usa atom_to_list/1 per convertirlo in una lista di caratteri
  erlang:atom_to_list(Input).

%% @doc Concatenates GameID and PlayerID to create a unique process name.
concat_game_player(GameID, PlayerID)  ->
  list_to_atom(to_str(GameID) ++ "_" ++ to_str(PlayerID)).