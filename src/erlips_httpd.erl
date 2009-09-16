%%%----------------------------------------------------------------------
%%%
%%% erlips @copyright 2009 
%%%
%%% @author litaocheng@gmail.com 
%%% @doc the erlips app framework, handle the http request
%%%
%%%----------------------------------------------------------------------
-module(erlips_httpd).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("erlips.hrl").

-export([start_link/0, start_link/2]).
-export([handle_request/2]).
-define(MAX_MOD_LEN, 255).

%% @doc start the http server
start_link() ->
    load_mods_name(),
    DocRoot = ?CONF_GET2(doc_root, "."),
    Ip = ?CONF_GET2(ip, "."),
    Port = ?CONF_GET2(port, 80),
    ?DEBUG2("start the ~p, ~p:~p doc root: ~p~n", [?MODULE, Ip, Port, DocRoot]),
    start_link([{name, ?MODULE}, {ip, Ip}, {port, Port}], DocRoot).

%% @doc start the http server
start_link(Opts, DocRoot) ->
    Loop = fun(Req) -> ?MODULE:handle_request(Req, DocRoot) end,
    mochiweb_http:start([{loop, Loop} | Opts]).

%% @doc handle the http request
-spec handle_request(Req :: any(), DocRoot :: string()) -> 'ok'.
handle_request(Req, DocRoot) ->
    Method = Req:get(method), 
    Path = "/" ++ Path0 = Req:get(path),

    {Main, Minor} = Req:get(version),
    ?INFO2("~s ~s HTTP ~B.~B ", [Method, Path, Main, Minor]),

    %% handle the request
    Resp = 
    case get_handle_mod(convert_path(Path0)) of
        {ok, Mod} ->
            {Code, Headers, Data} =
            case catch  Mod:handle(Req, Method) of
                {'EXIT', Error} ->
                    error_to_rsp(Error);
                {_Code, _Headers, _Data} = Rsp ->
                    Rsp
            end,
            %?DEBUG2("code:~p Headers:~p Data:~p", [Code, Headers, Data]),
            send_respond(Req, Code, Headers, Data);
        {error, _} ->
            ?DEBUG2("try send static file path:~p root:~p", [Path0, DocRoot]),
            Req:serve_file(Path0, DocRoot)
    end,

    ?INFO2("response ~s - ~s ~s ~B", [Req:get(peer)
                        , Req:get(method)
                        , Path
                        , Resp:get(code)]).


%%
%% internal API
%% 

%% convert some path: "" -> index
convert_path("") ->
    "index";
convert_path(P) ->
    P.

%% acrodding th path, get the handle module
get_handle_mod(Path) ->
    case get_handle_mod(Path, [], 0) of
        {ok, ModStr} ->
            case catch erlang:list_to_existing_atom(ModStr) of
                {'EXIT', {badarg, _}} ->
                    ?DEBUG2("to existing atom error", []),
                    {error, no_handle_mod};
                Mod ->
                    {ok, Mod}
            end;
        Other ->
            Other 
    end.


get_handle_mod(_Path, _Acc, AccLen) when AccLen >= ?MAX_MOD_LEN->
    {error, max_mod_len};
get_handle_mod("", Acc, _AccLen) ->
    {ok, lists:append(lists:reverse(Acc))};
get_handle_mod(Path, Acc, AccLen) ->
    {Part, Rest} =  mochiweb_util:path_split(Path),
    Acc2 = [Part, "_" | Acc],
    AccLen2 = AccLen + length(Part) + 1,
    get_handle_mod(Rest, Acc2, AccLen2).


error_to_rsp(Error) ->
    ?DEBUG2("error is~p", [Error]),
    {500, "", <<"500 Internal Error">>}.

%%  send data to client
send_respond(Req, Code, Headers, Data) ->
    %?DEBUG2("respond...~p~n ~p~n ~p~n ~p~n.~n", [Req, Code, Headers, Data]),
    DefaultHeaders = [
        {<<"Content-Type">>, negotiate_content_type(Req)},
        {<<"Cache-Control">>, <<"must-revalidate">>}
    ] ++ server_header(),    
    Req:respond({Code, Headers ++ DefaultHeaders, Data}).

negotiate_content_type(Req) ->
    %% Determine the appropriate Content-Type header for a JSON response
    %% depending on the Accept header in the request. A request that explicitly
    %% lists the correct JSON MIME type will get that type, otherwise the
    %% response will have the generic MIME type "text/plain"
    AcceptedTypes =
    case Req:get_header_value("Accept") of
        undefined       ->
            [];
        AcceptHeader    -> 
            string:tokens(AcceptHeader, ", ")
    end,
    case lists:member("application/json", AcceptedTypes) of
        true  -> "application/json";
        false -> "text/plain;charset=utf-8"
    end.

server_header() ->
    OTPVersion = "R" ++ integer_to_list(erlang:system_info(compat_rel)) ++ "B",
    [{<<"Server">>, ["erlips", "(Erlang OTP/", OTPVersion, ")"]}].


%% doc load all http handler mods name in ebin
load_mods_name() ->
    Dirs = ?CONF_GET2(http_handler_dirs, ["./ebin"]),

    Mods = 
    [
        [begin
            ModStr = filename:basename(F, ".beam"),
            list_to_atom(ModStr)
        end
        || F <- filelib:wildcard("_*.beam", Dir)]
     || Dir <- Dirs],
    
    ?DEBUG2("http handler mods: ~p~n", [lists:append(Mods)]),
    ok.
