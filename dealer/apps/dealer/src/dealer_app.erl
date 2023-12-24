%%%-------------------------------------------------------------------
%% @doc dealer public API
%% @end
%%%-------------------------------------------------------------------

-module(dealer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dealer:start_link().

stop(_State) ->
    ok.

%% internal functions
