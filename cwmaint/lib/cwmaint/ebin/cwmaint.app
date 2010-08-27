%% This is the application resource file (.app file) for the cwmaint,
%% application.
{application, cwmaint, 
  [{description, "Perform maintenance on ContentWatch tables"},
   {vsn, "0.1.0"},
   {modules, [cwm_app,
              cwm_sup1,
		    cwm_sup2,
		    cwm_master,
		    cwm_slave]},
   {registered,[cwm_sup1]},
   {applications, [kernel, stdlib, sasl, simple_cache]},
   {mod, {cwm_app,[]}},
   {start_phases, []}]}.

