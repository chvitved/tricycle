{application, tricycle,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             tricycle_app,
             tricycle_sup,
             tricycle_server
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { tricycle_app, []}},
  {env, []}
 ]}.
