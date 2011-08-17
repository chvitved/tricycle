{application, tricycle,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             tricycle_app,
             tricycle_sup,
             tricycle_server,
             tricycle_handle_commands
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  inets
                 ]},
  {mod, { tricycle_app, []}},
  {env, []}
 ]}.
