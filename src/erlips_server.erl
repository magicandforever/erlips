%%%----------------------------------------------------------------------
%%%
%%% xiaonei.com copyright © 2009 
%%%
%%% @author yourmail@opi-corp.com
%%% @doc demo server
%%%
%%%----------------------------------------------------------------------
-module(erlips_server).
-author('yourmail@gmail.com').
-vsn('0.1').
-behaviour(gen_server).
-include("erlips.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).
                            

%% @doc start the server
-spec start_link() -> {'ok', any()} | 'ignore' | {'error', any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% gen_server callbacks
%%
init(_Args) ->    
    {ok, []}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%
%% internal API
%%
