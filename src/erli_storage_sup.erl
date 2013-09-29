%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc The erli_storage supervisor.
%%% @end
%%%==========================================================

-module(erli_storage_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%-----------------------------------------------------------
%% API Methods
%%-----------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%-----------------------------------------------------------
%% Supervisor Callbacks
%%-----------------------------------------------------------

init(_Args) ->
    {ok, StorageBackend} = application:get_env(erli_storage, storage_backend),
    StorageProc = {erli_storage_backend,
		   {StorageBackend, init_and_link, []},
		   permanent, 5000, worker, [StorageBackend]},
    {ok, {{one_for_one, 10, 10}, StorageProc}}.
