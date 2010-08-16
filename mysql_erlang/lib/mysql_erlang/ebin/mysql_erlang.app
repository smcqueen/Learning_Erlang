%% This is the application resource file (.app file) for the mysql_erlang,
%% application.
{application, mysql_erlang, 
  [{description, "MySQL Library"},
   {vsn, "0.1.0"},
   {modules, [
		mysql_erlang,
		mysql_erlang_app,
		mysql_erlang_sup,
		mysql_erlang_auth,
		mysql_erlang_conn,
		mysql_erlang_recv
	     ]
   },
 
   {registered,[mysql_erlang_sup]},
   {applications, [kernel, stdlib, sasl, crypto]},
   {mod, {mysql_erlang_app,[]}},
   {start_phases, []}]}.

