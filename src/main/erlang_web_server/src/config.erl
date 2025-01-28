%%%-------------------------------------------------------------------
%%% @author Saverio
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. gen 2025 11:28
%%%-------------------------------------------------------------------
-author("Saverio").

%% Constants for the web server
-module(config).
-export([get_port/0 , java_server_url/0]).

%% Returns the port number for the web server
get_port() ->
  8080.

%% Define the Java server URL as a constant
java_server_url() ->
  "http://java-server-url".
