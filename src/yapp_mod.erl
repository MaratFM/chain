%%% -------------------------------------------------------------------
%%% Author  : marat
%%% Description :
%%%
%%% Created : 16.10.2011
%%% -------------------------------------------------------------------
-module(yapp_mod).

-include("yaws_api.hrl").
-export([out/1]).

%% ====================================================================
%% External functions
%% ====================================================================

out(Arg) ->
    Uri = yaws_api:request_url(Arg),
    [_|Path] = string:tokens(Uri#url.path, "/"),
	Method = (Arg#arg.req)#http_request.method,
	out(Arg, Method, Path).

out(_Arg, 'GET', []) ->
    Chains = chain_detector:get_chains(),
	[{html, lists:flatten(io_lib:format("~p~n", [Chains]))}];

out(Arg, 'PUT', []) ->
	Num = binary_to_list(Arg#arg.clidata),
 	try 
		chain_detector:handle_number(list_to_integer(Num)),
		[{html, "OK"}]
    catch error:badarg ->
        error(400, "400 Bad Request", 
			  io_lib:format("PUT data must be an integer, but it is ~p", [Num]))
    end;			

out(_Arg, _Method, Path) ->
	error(404, "404 Not Found", 
		  io_lib:format("The requested URL ~p was not found on this server.", [Path])).

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

error(Code, Title, Message) ->
	[{status, Code},
	 {header, {content_type, "text/html"}},
	 {ehtml, [{head, [], [{title, [], Title}]},
			  {body, [],
			   [{h1, [], Title},
				{p, [], Message}]}]}].