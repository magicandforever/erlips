%%%----------------------------------------------------------------------
%%%
%%% xiaonei.com copyright © 2009 
%%%
%%% @author yourmail@opi-corp.com
%%% @doc erlips header file
%%%
%%%----------------------------------------------------------------------

%% the log macro
-define(WARN2(F, D), io:format(F, D)).
-define(INFO2(F, D), io:format(F, D)).
-define(ERROR2(F, D), io:format(F, D)).
-define(FATAL2(F, D), io:format(F, D)).
-define(DEBUG2(F, D), io:format(F, D)).

%% the config macro
-define(CONF_GET2(K, Def), (
        try element(2, application:get_env(erlips, K))
        catch _:_ ->
            Def
        end)).
                
