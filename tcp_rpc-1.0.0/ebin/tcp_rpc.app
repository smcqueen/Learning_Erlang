%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, tcp_rpc,
 [{description, "RPC server for Erlang and OTP in action"},
  {vsn, "1.0.0"},
  {modules, [tr_app,
             tr_sup]},
  {registered, [tr_sup]},
  {applications, [kernel, stdlib]},
  {mod, {tr_app, []}}
 ]}.
