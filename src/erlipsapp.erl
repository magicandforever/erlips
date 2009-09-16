%%%----------------------------------------------------------------------
%%%
%%% erlips @copyright 2009 
%%%
%%% @author litaocheng@gmail.com 
%%% @doc erlips app and supervisor callback
%%%
%%%----------------------------------------------------------------------
-module(erlipsapp).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("erlips.hrl").

-behaviour(application).
-behaviour(supervisor).

-export([start/1]).
-export([start/2, stop/1]).
-export([init/1]).


%% @doc start the application from the erl shell
-spec start(Args :: list()) -> 'ok' | {'error', any()}.
start([UseCaclib]) ->
    ensure_apps(),
    ?DEBUG2("start the ~p~n", [?MODULE]),
    erlips_ctl:init(),
    init_caclib(UseCaclib),
    application:start(erlips).

%% @doc the application start callback
-spec start(Type :: any(), Args :: any()) -> any().
start(_Type, _Args) ->
    ?DEBUG2("start the supervisor sup ~n", []),
    supervisor:start_link({local, erlips_sup}, ?MODULE, []).

%% @doc the application  stop callback
stop(_State) ->
    ok.

%% @doc supervisor callback
init(_Args) -> 
    Stragegy = {one_for_one, 10, 10},

    ModGeoip = {egeoip, {egeoip, start, []},
                permanent, 2000, worker, [egeoip]},
    ModHttpd = {erlips_httpd, {erlips_httpd, start_link, []},
                permanent, 2000, worker, [erlips_httpd]},

    {ok, {Stragegy, [
                    ModGeoip,
                    ModHttpd 
                    ]}
    }.

%%
%% internal API
%%

%% first ensure some apps must start
ensure_apps() ->
    application:start(sasl),
    ok.

%% init the caclib
init_caclib(false) -> ok;
init_caclib(true)->
   ?DEBUG2("start the caclibapp!~n", []),
    caclibapp:start().
