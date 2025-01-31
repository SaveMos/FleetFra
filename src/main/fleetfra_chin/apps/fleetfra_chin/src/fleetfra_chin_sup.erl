%%%-------------------------------------------------------------------
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc fleetfra_chin top level supervisor.
%% This module defines a supervisor for the fleetfra_chin system.
%% @end
%%%-------------------------------------------------------------------

-module(fleetfra_chin_sup).
-author("SaveMos").
-behaviour(supervisor).

-export([start_link/0]).  % Export the start_link function
-export([init/1]).        % Export the init function

-define(SERVER, ?MODULE).  % Define SERVER as the module name for easy reference

%% start_link/0 starts the supervisor process.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% init/1 initializes the supervisor, setting up its flags and child specifications.
%%
%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  % Define supervisor flags
  SupFlags = #{strategy => one_for_all,  % Strategy: all children must succeed/fail together
    intensity => 0,           % Optional intensity level (not used in this case)
    period => 1},             % Period in seconds (not used in this case)

  % No child processes to supervise, so we set up an empty child specification list
  ChildSpecs = [],

  {ok, {SupFlags, ChildSpecs}}.  % Return the supervisor configuration

%% internal functions
