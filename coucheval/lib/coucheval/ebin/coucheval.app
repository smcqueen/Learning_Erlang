%% This is the application resource file (.app file) for the coucheval,
%% application.
{application, coucheval, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [coucheval_app,
              coucheval_sup]},
   {registered,[coucheval_sup]},
   {applications, [kernel, stdlib]},
   {mod, {coucheval_app,[]}},
   {start_phases, []}]}.

