%% This is the application resource file (.app file) for the http_interface,
%% application.
{application, http_interface, 
  [{description, "RESTful interface to simple_cache"},
   {vsn, "0.1.0"},
   {modules, [hi_app,
              hi_sup,
	      hi_server]},
   {registered,[hi_sup]},
   {applications, [kernel, stdlib]},
   {mod, {hi_app,[]}},
   {start_phases, []}]}.

