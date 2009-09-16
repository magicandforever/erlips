%%%----------------------------------------------------------------------
%%%
%%% wg, Copyright (C) 2009 
%%%
%%% @author litaocheng@gmail.com
%%% @doc the demo module handle the request path:
%%%  "http://host/demo
%%%----------------------------------------------------------------------
-module('_demo').
-author('ltiaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").

-export([handle/2]).

handle(Req, Method) ->
    ?Debug2("handle request :~p ~n", [Req]),
    IpStr = Req:get(peer), 
    Headers = Req:get(headers),

    ReqHeaders =
    lists:foldr(fun({K, V}, Acc) ->
        [misc_util:any_to_list(K), " : ", misc_util:any_to_list(V), "\n" | Acc]
    end,
    [],
    mochiweb_headers:to_list(Headers)),

    Query = Req:parse_qs(),
    Post = Req:parse_post(),

    RspStr = 
    io_lib:format(<<
            "It Works!~n~nHello, Your IP is ~s~nmethod: ~s~nqurey string:~p~npost body:~p~nRequest Data:~n">>,
            [IpStr, Method, Query, Post]),

    ?Debug2("response is:~p ~n", [RspStr]),
    {200, [], [RspStr, ReqHeaders]}.
