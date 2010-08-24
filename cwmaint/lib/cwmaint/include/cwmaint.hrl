-define(ORGCHILD, {cwm_org, {cwm_org, start_link, []},
		permanent, 2000, worker, [cwm_org]}).

-define(ACTCHILD, {cwm_act, {cwm_act, start_link, []},
		temporary, brutal_kill, worker, [cwm_act]}).
