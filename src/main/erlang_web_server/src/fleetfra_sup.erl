%%%-------------------------------------------------------------------
%%% @author Saverio
%%% @copyright (C) 2025, <FleetFra>
%%% @doc
%%%
%%% @end
%%% Created : 28. gen 2025 11:27
%%%-------------------------------------------------------------------
-module(fleetfra_sup).
-behaviour(supervisor).
-author("Saverio").
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  {ok, {{one_for_one, 5, 10}, [
    {fleetfra_app, {fleetfra_app, start, []}, permanent, 1000, worker, [fleetfra_app]}
  ]}}.


