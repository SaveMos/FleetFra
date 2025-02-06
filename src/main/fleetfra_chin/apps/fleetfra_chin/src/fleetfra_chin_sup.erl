%%==============================================================================%%
%% @author SaveMos
%% @copyright (C) 2025, <FleetFra>
%% @doc fleetfra_chin top-level supervisor.
%% This module defines a supervisor for the fleetfra_chin system.
%% @end
%%==============================================================================%%

-module(fleetfra_chin_sup).
-author("SaveMos").
-behaviour(supervisor).

-export([start_link/0]).  % Export the start_link function to initiate the supervisor
-export([init/1]).        % Export the init function to initialize the supervisor

-define(SERVER, ?MODULE).  % Define SERVER as the module name for easy reference

%% start_link/0 starts the supervisor process.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% init/1 initializes the supervisor, setting up its flags and child specifications.
%%
%% sup_flags() = #{strategy => strategy(),         % optional supervisor strategy (e.g., one_for_all, rest_for_one, etc.)
%%                 intensity => non_neg_integer(), % optional intensity of strategy (not used here)
%%                 period => pos_integer()}      % optional period for strategy application (not used here)
%%
%% child_spec() = #{id => child_id(),       % mandatory child identifier
%%                  start => mfargs(),      % mandatory arguments for child process start
%%                  restart => restart(),   % optional restart strategy (e.g., permanent, transient, temporary)
%%                  shutdown => shutdown(), % optional shutdown strategy (time before shutdown)
%%                  type => worker(),       % optional type of child process (worker or supervisor)
%%                  modules => modules()}   % optional list of modules to load for the child
init([]) ->
  % Define supervisor flags and child specifications
  SupFlags = #{strategy => one_for_all,  % Supervisor strategy: all children must succeed/fail together
    intensity => 0,           % Optional intensity level (not used in this case)
    period => 1},             % Period in seconds (not used in this case)

  % No child processes to supervise, so we define an empty list of child specifications
  ChildSpecs = [],

  {ok, {SupFlags, ChildSpecs}}.  % Return the supervisor configuration, indicating successful initialization

%% internal functions
