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
     {env, []}
    ]
}.
