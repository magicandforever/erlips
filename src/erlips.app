{application, erlips,
    [{description, "some description"},
     {vsn, "0.1"},
     {modules, [
                erlipsapp,
                erlips_server
                ]},
     {registered, []},
     {applications, [kernel, stdlib, sasl]},
     {mod, {erlipsapp, []}},
     {env, [
             {doc_root, "./www"},
             {ip, "0.0.0.0"},
             {port, 8080},
             {max, 5000},
             {http_handler_dirs, ["./ebin"]}
         ]}
    ]
}.
