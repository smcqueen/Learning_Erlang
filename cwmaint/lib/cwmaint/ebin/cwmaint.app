%% This is the application resource file (.app file) for the cwmaint,
%% application.
{application, cwmaint, 
  [{description, "Perform maintenance on ContentWatch tables"},
   {vsn, "0.1.0"},
   {modules, [cwmaint_app,
              cwmaint_sup,
	      cwmaint_srv]},
   {registered,[cwmaint_sup]},
   {applications, [kernel, stdlib, sasl, simple_cache]},
   {mod, {cwmaint_app,[]}},
   {start_phases, []}]}.

