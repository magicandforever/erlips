%%%----------------------------------------------------------------------
%%%
%%% erlips @copyright 2009 
%%%
%%% @author litaocheng@gmail.com 
%%% @doc the module handle the request path:
%%%  "http://host/ips/geoip
%%%
%%%----------------------------------------------------------------------
-module('_ips_geoip').
-author('ltiaocheng@gmail.com').
-vsn('0.1').
-include("erlips.hrl").

-define(GET2(K, L), proplists:get_value(K, L)).
-define(GET3(K, L, Def), proplists:get_value(K, L, Def)).

-export([handle/2]).

%% @doc handle the /ips/geoip request
-spec handle(Req :: any(), Method :: atom()) -> {pos_integer(), list(), iodata()}.
handle(Req, 'GET') ->
    IpStr = Req:get(peer), 
    Query = Req:parse_qs(),
    QueryIp = ?GET3("ip", Query, IpStr),
    {200, [], [QueryIp]}.
    
    

