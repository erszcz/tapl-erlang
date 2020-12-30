%%%-------------------------------------------------------------------
%% @doc tapl public API
%% @end
%%%-------------------------------------------------------------------

-module(tapl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tapl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
