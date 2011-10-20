%%% -------------------------------------------------------------------
%%% Author  : marat
%%% Description :
%%%
%%% Created : 16.10.2011
%%% -------------------------------------------------------------------
-module(chain_detector).
-behaviour(gen_server).

-export([start_link/0, handle_number/1, get_chains/0, test/0]). %% external
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% gen_server

-record(state, {
	prev=0,
	chain=[],
	longest=[]
}).

-define(SERVER, ?MODULE).
-include("settings.hrl").

%% ====================================================================
%% External functions
%% ====================================================================

%%---------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%---------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: handle_number(Num) -> ok
%% Description: Handle number, detects and safe chains
%%--------------------------------------------------------------------
handle_number(Num) ->
  gen_server:cast(?SERVER, {handle_number, Num}).

%%--------------------------------------------------------------------
%% Function: get_chains() -> {ok, Chains} | {error, Reason}
%% Description: Return longest chains
%%--------------------------------------------------------------------
get_chains() ->
  gen_server:call(?SERVER, {get_chains}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{longest=lists:duplicate(?MAX_CHAINS, {0, undefined})}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({get_chains}, _From, State) ->
  	{reply, element(2, lists:unzip(State#state.longest)), State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({handle_number, Num}, State) when Num > State#state.prev ->
    {noreply, State#state{prev=Num, chain=[Num|State#state.chain]}};

handle_cast({handle_number, Num}, State) ->	
    {noreply, State#state{prev=Num, chain=[Num], 
						  longest=update_chains(lists:reverse(State#state.chain), State#state.longest)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: update_chains(Chain, Longest) -> Longest
%% Description: Update list of Longest chains with Chain  
%%--------------------------------------------------------------------
update_chains(Chain, Longest) ->
	ChainLen = length(Chain),
	[{MinLen, _}|LongestTail] = Longest, 
	if 
		ChainLen > MinLen ->
			lists:keysort(1, [{ChainLen, Chain}|LongestTail]);
		true ->
			Longest
	end.



test() -> 
	random:seed(erlang:now()),
	Gen = lists:map(fun(_) -> random:uniform(1000) end, lists:seq(1,300)),
	io:format("Initial: ~p ~n", [Gen]),
	start_link(),
	lists:foreach(fun(X)->handle_number(X) end, Gen),
	Res = get_chains(),
	io:format("Result: ~p ~n", [Res]),
	gen_server:call(?SERVER, stop),
	Res.