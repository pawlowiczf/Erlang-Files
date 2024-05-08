%%%-------------------------------------------------------------------
%% @doc pollutionOTP public API
%% @end
%%%-------------------------------------------------------------------

-module(pollutionOTP_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pollutionOTP_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
