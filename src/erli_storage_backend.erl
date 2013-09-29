%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc Behviour that defines an interface for erli_storage
%%% backends.
%%% @end
%%%==========================================================

-module(erli_storage_backend).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init_and_link, 1}, {write, 1}, {read, 2}, {read_bulk, 2},
     {delete, 1}, {count, 1}];
behaviour_info(_) ->
    undefined.
