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
-export([bin_to_atom/1, to_str/1]).

%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc
%% Cast a binary object into an atom.
%% @end
%%-------------------------------------------------------------------
bin_to_atom(Bin) ->
  case is_binary(Bin) of
    true -> binary_to_atom(Bin, utf8);
    false -> Bin
  end.

to_str(Input) when is_binary(Input) ->
  % Se l'input è un binary, usa erlang:binary_to_list/1 per convertirlo in una lista di caratteri
  binary:bin_to_list(Input);
to_str(Input) when is_atom(Input) ->
  % Se l'input è un atomo, usa atom_to_list/1 per convertirlo in una lista di caratteri
  atom_to_list(Input).
