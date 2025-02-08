%%%===============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%%% @doc
%%% This module provides utility functions for data transformation, including:
%%% - Converting a binary object to an atom.
%%% - Converting different data types into a string representation.
%%% - Concatenating game and player identifiers into a unique atom.
%%%
%%% The functions handle various data types, ensuring proper type conversions.
%%% @end
%%% Created : 06. Feb 2025 12:16
%%%===============================================================================%%
-module(utility).
-author("Saverio").

%% API
-export([bin_to_atom/1, to_str/1, concat_game_player/2]).

%%-------------------------------------------------------------------
%% @doc Converts a binary object into an atom.
%%
%% @param Bin A binary value to be converted.
%% @returns If Bin is a binary, it is converted to an atom using UTF-8 encoding.
%%         Otherwise, it is returned unchanged.
%% @end
%%-------------------------------------------------------------------
bin_to_atom(Bin) ->
  case erlang:is_binary(Bin) of
    true -> erlang:binary_to_atom(Bin, utf8);
    false -> Bin
  end.

%%-------------------------------------------------------------------
%% @doc Converts an input value into a string (character list).
%%
%% @param Input Can be a binary, list, or atom.
%% @returns If Input is a binary, it is converted to a list of characters.
%%         If Input is already a list, it is returned unchanged.
%%         If Input is an atom, it is converted to a character list.
%% @end
%%-------------------------------------------------------------------
to_str(Input) when erlang:is_binary(Input) ->
  % If the input is a binary, convert it to a character list.
  binary:bin_to_list(Input);
to_str(Input) when erlang:is_list(Input) ->
  % If the input is a list, return it as is.
  Input;
to_str(Input) when erlang:is_atom(Input) ->
  % If the input is an atom, convert it to a character list.
  erlang:atom_to_list(Input).

%%-------------------------------------------------------------------
%% @doc Concatenates GameID and PlayerID to create a unique process name.
%%
%% @param GameID The identifier of the game (can be an atom, binary, or list).
%% @param PlayerID The identifier of the player (can be an atom, binary, or list).
%% @returns An atom representing the concatenation of GameID and PlayerID, separated by an underscore.
%% @end
%%-------------------------------------------------------------------
concat_game_player(GameID, PlayerID)  ->
  erlang:list_to_atom(to_str(GameID) ++ "_" ++ to_str(PlayerID)).