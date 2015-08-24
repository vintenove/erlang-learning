{application, ppool,
[{vsn, "1.0.0"},
 {modules, [ppool, ppool_serv, ppool_sup, ppool_super_sup, ppool_worker_sup, ppool]},
 {registered, [ppool]},
 {mod, {ppool, []}}]}.