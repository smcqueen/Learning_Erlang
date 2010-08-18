%% This is the application resource file (.app file) for the cwmaint,
%% application.
{application, cwmaint, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [cwmaint_app,
              cwmaint_sup]},
   {registered,[cwmaint_sup]},
   {applications, [kernel, stdlib]},
   {mod, {cwmaint_app,[]}},
   {start_phases, []}]}.

