%% This is the application resource file (.app file) for the ti,
%% application.
{application, tcp_interface, 
  [{description, "tcp_interface application"},
   {vsn, "0.1.0"},
   {modules, [ti_app,
   	      ti_sup,
	      ti_server
   	     ]},
   {registered,[ti_sup]},
   {applications, [kernel, stdlib]},
   {mod, {ti_app,[]}},
   {start_phases, []}]}.

