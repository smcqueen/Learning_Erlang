%% This is the application resource file (.app file) for the gen_web_server,
%% application.
{application, gen_web_server, 
  [{description, "generic web server"},
   {vsn, "0.1.0"},
   {modules, [gen_web_server,
              gws_connection_sup,
	      gws_server]},
   {registered,[gen_web_server_sup]},
   {applications, [kernel, stdlib]},
   {mod, {gen_web_server,[]}},
   {start_phases, []}]}.

