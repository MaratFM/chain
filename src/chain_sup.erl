%%% -------------------------------------------------------------------
%%% Author  : marat
%%% Description :
%%%
%%% Created : 16.10.2011
%%% -------------------------------------------------------------------
-module(chain_sup).
-behaviour(supervisor).

-export([start_link/0]). %% external
-export([init/1]). %% supervisor

-define(SUPERVISOR, ?MODULE).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Func: start_link/0
%% Returns: {ok, pid()} |
%%          ignore      |
%%          {error, Reason}
%% --------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% ====================================================================
%% Supervisor functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, {SupFlags,  [ChildSpec]}} |
%%          ignore                         |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	Procs = [{chain_detector, {chain_detector, start_link, []},
			  permanent, 5000, worker, dynamic}],
	{ok, {{one_for_one, 10, 10}, Procs}}.