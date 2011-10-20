%% Author: marat
%% Created: Oct 17, 2011
%% Description: 
%% Usage:
%% Pids = generators:start(5).
%% generators:stop(Pids).

-module(generators).

-export([start/1, stop/1, generator/0]).

-define(URL, "http://127.0.0.1:8000/chain/").
-define(MIN_TIMEOUT, 100).
-define(MAX_TIMEOUT, 1000).
-define(MAX_NUMBER, 1000).

generator() ->
	random:seed(now()),
	inets:start(),
	loop(100).

loop(TimeOut) ->
    receive
    	stop -> stop
    after TimeOut ->
		NewTimeOut = random:uniform(?MAX_TIMEOUT-?MIN_TIMEOUT) + ?MIN_TIMEOUT,
        Num = random:uniform(?MAX_NUMBER),                  
		Data = list_to_binary(integer_to_list(Num)),
        Response = httpc:request(put, {?URL, [], [], Data}, [], []),			
		case Response of
		     {ok, {_, _, Body}} -> io:format("Pid: ~p send ~p -> ~p, wait ~p msec~n", [self(), Num, Body, TimeOut]);							                       
		                      _ -> io:format("Pid: ~p send ~p -> ERROR, wait ~p msec~n", [self(), Num, TimeOut])			
		end,
		loop(NewTimeOut)
    end.

start(Count) -> 
	start(Count, []).

start(0, Pids) -> 
	Pids;

start(Count, Pids) -> 
	Pid = spawn(generators, generator, []),
    io:format("Start generator with Pid = ~p~n", [Pid]),
	start(Count-1, [Pid|Pids]).


stop([]) -> ok;

stop([Pid | Pids]) ->
	Pid ! stop,
    io:format("Stop generator with Pid = ~p~n", [Pid]),
    stop(Pids). 
						   
