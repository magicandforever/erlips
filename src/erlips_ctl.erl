%%%----------------------------------------------------------------------
%%%
%%%
%%% @author litaocheng@gmail.com
%%% @doc the erlips ctl module
%%%----------------------------------------------------------------------
-module(erlips_ctl).
-author('litaocheng@gmail.com').

-export([start/0,
	 init/0,
	 process/1,
	 dump_to_textfile/1,
	 register_commands/3,
	 register_commands/4,
	 unregister_commands/3,
	 unregister_commands/4]).

-include("erlips_ctl.hrl").

start() ->
    case init:get_plain_arguments() of
	[SNode | Args]->
	    %io:format("plain arguments is:~n~p", [AArgs]),
	    SNode1 = case string:tokens(SNode, "@") of
		[_Node, _Server] ->
		    SNode;
		_ ->
		    case net_kernel:longnames() of
			 true ->
			     SNode ++ "@" ++ inet_db:gethostname() ++
				      "." ++ inet_db:res_option(domain);
			 false ->
			     SNode ++ "@" ++ inet_db:gethostname();
			 _ ->
			     SNode
		     end
	    end,
	    Node = list_to_atom(SNode1),
	    Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			 {badrpc, Reason} ->
			     ?PRINT("RPC failed on the node ~p: ~p~n",
				       [Node, Reason]),
			     ?STATUS_BADRPC;
			 S ->
			     S
		     end,
	    halt(Status);
	_ ->
	    print_usage(),
	    halt(?STATUS_USAGE)
    end.

init() ->
    ets:new(node_ctl_cmds, [named_table, set, public]),
    ets:new(node_ctl_host_cmds, [named_table, set, public]).


process(["status"]) ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    ?PRINT("Node ~p is ~p. Status: ~p~n",
              [node(), InternalStatus, ProvidedStatus]),
    case lists:keysearch(erlips, 1, application:which_applications()) of
        false ->
            ?PRINT("node is not running~n", []),
            ?STATUS_ERROR;
        {value,_Version} ->
            ?PRINT("node is running~n", []),
            ?STATUS_SUCCESS
    end;

process(["stop"]) ->
    init:stop(),
    ?STATUS_SUCCESS;

process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;

process(["mnesia"]) ->
    ?PRINT("~p~n", [mnesia:system_info(all)]),
    ?STATUS_SUCCESS;

process(["mnesia", "info"]) ->
    mnesia:info(),
    ?STATUS_SUCCESS;

process(["mnesia", Arg]) when is_list(Arg) ->
    case catch mnesia:system_info(list_to_atom(Arg)) of
	{'EXIT', Error} -> ?PRINT("Error: ~p~n", [Error]);
	Return -> ?PRINT("~p~n", [Return])
    end,
    ?STATUS_SUCCESS;

process(["delete-old-messages", Days]) ->
    case catch list_to_integer(Days) of
	{'EXIT',{Reason, _Stack}} ->
            ?PRINT("Can't delete old messages (~p). Please pass an integer as parameter.~n",
                      [Reason]),
	    ?STATUS_ERROR;
	Integer when Integer >= 0 ->
	    {atomic, _} = mod_offline:remove_old_messages(Integer),
	    ?PRINT("Removed messages older than ~s days~n", [Days]),
	    ?STATUS_SUCCESS;
	_Integer ->
	    ?PRINT("Can't delete old messages. Please pass a positive integer as parameter.~n", []),
	    ?STATUS_ERROR
    end.

print_usage() ->
    CmdDescs =
	[{"status", "get node status"},
	 {"stop", "stop node"},
	 {"restart", "restart node"},
	 {"mnesia [info]", "show information of Mnesia system"}
	 ] ++
	ets:tab2list(node_ctl_cmds),
    MaxCmdLen =
	lists:max(lists:map(
		    fun({Cmd, _Desc}) ->
			    length(Cmd)
		    end, CmdDescs)),
    NewLine = io_lib:format("~n", []),
    FmtCmdDescs =
	lists:map(
	  fun({Cmd, Desc}) ->
		  ["  ", Cmd, string:chars($\s, MaxCmdLen - length(Cmd) + 2),
		   Desc, NewLine]
	  end, CmdDescs),
    ?PRINT(
      "Usage: nodectl [--node nodename] command [options]~n"
      "~n"
      "Available commands in this node node:~n"
      ++ FmtCmdDescs ++
      "~n"
      "Examples:~n"
      "  nodectl restart~n"
      "  nodectl --node node@host restart~n"
      "  nodectl vhost jabber.example.org ...~n",
     []).

register_commands(CmdDescs, Module, Function) ->
    ets:insert(node_ctl_cmds, CmdDescs),
    node_hooks:add(node_ctl_process,
		       Module, Function, 50),
    ok.

register_commands(Host, CmdDescs, Module, Function) ->
    ets:insert(node_ctl_host_cmds,
	       [{{Host, Cmd}, Desc} || {Cmd, Desc} <- CmdDescs]),
    node_hooks:add(node_ctl_process, Host,
		       Module, Function, 50),
    ok.

unregister_commands(CmdDescs, Module, Function) ->
    lists:foreach(fun(CmdDesc) ->
			  ets:delete_object(node_ctl_cmds, CmdDesc)
		  end, CmdDescs),
    node_hooks:delete(node_ctl_process,
			  Module, Function, 50),
    ok.

unregister_commands(Host, CmdDescs, Module, Function) ->
    lists:foreach(fun({Cmd, Desc}) ->
			  ets:delete_object(node_ctl_host_cmds,
					    {{Host, Cmd}, Desc})
		  end, CmdDescs),
    node_hooks:delete(node_ctl_process,
			  Module, Function, 50),
    ok.

dump_to_textfile(File) ->
    dump_to_textfile(mnesia:system_info(is_running), file:open(File, write)).
dump_to_textfile(yes, {ok, F}) ->
    Tabs1 = lists:delete(schema, mnesia:system_info(local_tables)),
    Tabs = lists:filter(
	     fun(T) ->
		     case mnesia:table_info(T, storage_type) of
			 disc_copies -> true;
			 disc_only_copies -> true;
			 _ -> false
		     end
	     end, Tabs1),
    Defs = lists:map(
	     fun(T) -> {T, [{record_name, mnesia:table_info(T, record_name)},
			    {attributes, mnesia:table_info(T, attributes)}]} 
	     end,
	     Tabs),
    io:format(F, "~p.~n", [{tables, Defs}]),
    lists:foreach(fun(T) -> dump_tab(F, T) end, Tabs),
    file:close(F);
dump_to_textfile(_, {ok, F}) ->
    file:close(F),
    {error, mnesia_not_running};
dump_to_textfile(_, {error, Reason}) ->
    {error, Reason}.


dump_tab(F, T) ->
    W = mnesia:table_info(T, wild_pattern),
    {atomic,All} = mnesia:transaction(
		     fun() -> mnesia:match_object(T, W, read) end),
    lists:foreach(
      fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, T)]) end, All).

