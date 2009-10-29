# !/bin/bash
#
Config = {sys, [{escript, "examples/display_args", [{incl_cond, include}]},
                {app, inets, [{incl_cond, include}]},
                {app, mnesia, [{incl_cond, exclude}]},
                {app, ssl, [{incl_cond, exclude}]},
                {app, runtime_tools, [{incl_cond, exclude}]},
                                                                                                               {app, syntax_tools, [{incl_cond, exclude}]}]}.

