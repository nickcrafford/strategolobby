{application, strategoserver,
 [{description, "strategoserver"},
  {vsn, "0.01"},
  {modules, [
    strategoserver,
    strategoserver_app,
    strategoserver_sup,
    strategoserver_web,
    strategoserver_deps
  ]},
  {registered, []},
  {mod, {strategoserver_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
