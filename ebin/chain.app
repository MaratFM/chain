{application, chain, [
	{description, "Chain counter application."},
	{vsn, "0.1.0"},
	{modules, []},
	{registered, []},
	{applications, [kernel,stdlib,yaws]},
	{mod, {chain_app, []}},
	{env, [
 		{yapp_appmods, [{"/", yapp_mod}]}		
	]}
]}.  