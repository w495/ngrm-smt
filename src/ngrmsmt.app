{application, ngrmsmt,
 [{description, "ngrm smt"},
  {vsn, "1"},
  {modules, [ngrmsmt_supervisor, ngrmsmt_gen_server, main, db_mnesia, sentences, ngrams, words]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {ngrmsmt,[]}}
 ]}.