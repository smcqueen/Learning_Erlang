%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, resource_discovery,
 [{description, "Application for resource discovery"},
  {vsn, "0.1.0"},
  {modules, [resource_discovery,
	resource_discovery_app,
             resource_discovery_sup]},
  {registered, [resource_discovery_sup]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {resource_discovery_app, []}}
 ]}.
