%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc The erli_storage application module.
%%% @end
%%%==========================================================

-module(erli_storage_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%-----------------------------------------------------------
%% Application Callbacks
%%-----------------------------------------------------------

start(_StartType, _StartArgs) ->
    case erli_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.
