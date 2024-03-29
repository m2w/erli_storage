%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc A mnesia based backend for erli_storage.
%%% @end
%%%==========================================================

-module(erli_storage_mnesia).
-behaviour(erli_storage_backend).

%% erli_storage_backend callbacks
-export([init_and_link/1, write/1, read/2, read_bulk/2, delete/1, count/1]).

-include("erli_models.hrl").


%%-----------------------------------------------------------
%% Types
%%-----------------------------------------------------------
-type new_obj() :: object().
-type existing_obj() :: object().
-type table_name() :: collection_type() | counters.


%%-----------------------------------------------------------
%% erli_storage_backend Callbacks
%%-----------------------------------------------------------

init_and_link(_Args) ->
    ignore.

-spec write(object()) -> object() | conflict_error() | mnesia_error().
write(Obj) when is_record(Obj, target) ->
    RecordNumber = mnesia:dirty_update_counter(counters, target, 1),
    LastModified = erli_utils:unix_timestamp(),
    Object = Obj#target{record_number=RecordNumber, last_modified=LastModified},
    case mnesia:dirty_index_read(targets, Object#target.url, url) of
	[] ->
	    case generate_id(targets) of
		{error, Error} ->
		    {error, Error};
		Id ->
		    UpdatedObject = Object#target{id=Id},
		    ok = mnesia:dirty_write(targets, UpdatedObject),
		    UpdatedObject
	    end;
	[ConflictingRecord] ->
	    {error, {conflict,
		     {Object#target{id= <<"none">>}, ConflictingRecord}}}
    end;
write(Obj) when is_record(Obj, path) ->
    RecordNumber = mnesia:dirty_update_counter(counters, path, 1),
    Object = Obj#path{record_number=RecordNumber},
    case read(target, Object#path.target_id) of
	{error, _Error} ->
	    {error, no_matching_target};
	_T ->
	    case Object#path.id of
		undefined ->
		    case generate_id(paths) of
			{error, Error} ->
			    {error, Error};
			Id ->
			    UpdatedObject = Object#path{id=Id},
			    ok = mnesia:dirty_write(paths, UpdatedObject),
			    UpdatedObject
		    end;
		Id ->
		    case read(path, Id) of
			{error, not_found} ->
			    ok = mnesia:dirty_write(paths, Object),
			    Object;
			{error, Error} ->
			    {error, Error};
			ConflictingObject ->
			    {error, {conflict,
				     {Object, ConflictingObject}}}
		    end
	    end
    end;
write(Obj) when is_record(Obj, visit) ->
    Id = mnesia:dirty_update_counter(counters, visit, 1),
    Time = erli_utils:unix_timestamp(),
    UpdatedObject = Obj#visit{id=erli_utils:int_to_bitstring(Id),
			      record_number=Id, time=Time},
    ok = mnesia:dirty_write(visits, UpdatedObject),
    UpdatedObject.


-spec read(object_type(), id()) -> object() | mnesia_error().
read(target, Id) ->
    wrap_read(mnesia:dirty_read(targets, Id));
read(path, Id) ->
    wrap_read(mnesia:dirty_read(paths, Id));
read(visit, Id) ->
    wrap_read(mnesia:dirty_read(visits, Id)).


-spec read_bulk(collection_type(), range()) -> collection().
read_bulk(targets, {Start, End}) ->
    mnesia:dirty_select(targets, [{#target{record_number='$1', _='_'},
				   [{'>=', '$1', Start},
				    {'=<', '$1', End}],
				   ['$_']}]);
read_bulk(paths, {Start, End}) ->
    mnesia:dirty_select(paths, [{#path{record_number='$1', _='_'},
				 [{'>=', '$1', Start},
				  {'=<', '$1', End}],
				 ['$_']}]);
read_bulk(visits, {Start, End}) ->
    mnesia:dirty_select(visits, [{#visit{record_number='$1', _='_'},
				  [{'>=', '$1', Start},
				   {'=<', '$1', End}],
				  ['$_']}]).


-spec delete(object()) ->
		    {request_accepted | target_banned, object()}.
delete(Object) when is_record(Object, target) ->
    CurrentLimit = erli_utils:get_env(flag_limit),
    LastModified = erli_utils:unix_timestamp(),

    case Object#target.flag_count of
	FC when FC < CurrentLimit ->
	    UpdatedObject = Object#target{flag_count=FC+1,
					  last_modified=LastModified},
	    mnesia:dirty_write(targets, UpdatedObject),
	    {request_accepted, UpdatedObject};
	FC when FC >= CurrentLimit ->
	    UpdatedObject = Object#target{last_modified=LastModified,
					  flag_count=FC+1,
					  is_banned=true},
	    ban(UpdatedObject),
	    {target_banned, UpdatedObject}
    end;
delete(Object) when is_record(Object, path) ->
    Target = read(target, Object#path.target_id),
    delete(Target).


-spec count(collection_type()) -> collection_size().
count(Collection) ->
    mnesia:table_info(Collection, size).

%%-----------------------------------------------------------
%% Utility API
%%-----------------------------------------------------------

-spec setup_tables([node()]) -> ok.
setup_tables(Nodes) ->
    ok = mnesia:start(), % ensure mnesia is started
    fix_schema(Nodes),
    create_tables(Nodes).

-spec targets_requiring_thumbnail() -> [#target{}].
targets_requiring_thumbnail() ->
    list_missing_thumbs() ++ list_outdated_thumbs().

-spec thumbnail_generated(#target{}) -> ok.
thumbnail_generated(Target) ->
    mnesia:dirty_write(targets,
		       Target#target{has_thumbnail=true,
				     last_modified=erli_utils:unix_timestamp()}).

%%-----------------------------------------------------------
%% Internal Methods
%%-----------------------------------------------------------

-spec list_missing_thumbs() -> [#target{}].
list_missing_thumbs() ->
    mnesia:dirty_select(targets, [{#target{has_thumbnail='$1', _='_'},
				   [{'=:=', '$1', false}], ['$_']}]).

-spec list_outdated_thumbs() -> [#target{}].
list_outdated_thumbs() ->
    Lim = erli_utils:get_env(thumbnail_age_limit),
    MaxAge = erli_utils:unix_timestamp() - Lim*24*60*60,
    mnesia:dirty_select(targets,
			[{#target{last_modified='$1', has_thumbnail='$2', _='_'},
			  [{'=<', '$1', MaxAge}, {'=:=', '$2', true}],
			  ['$_']}]).

-spec create_tables([node()]) -> ok.
create_tables(Nodes) ->
    maybe_create_table(counters, [{disc_copies, Nodes},
				  {attributes, [type, id]}]),
    maybe_create_table(targets, [{disc_copies, Nodes},
				 {record_name, target},
				 {index, [url, record_number]},
				 {attributes, record_info(fields, target)}]),
    maybe_create_table(paths, [{disc_copies, Nodes},
			       {record_name, path},
			       {index, [target_id, record_number]},
			       {attributes, record_info(fields, path)}]),
    maybe_create_table(visits, [{disc_copies, Nodes},
				{record_name, visit},
				{index, [path_id]},
				{attributes, record_info(fields, visit)}]).


-spec maybe_create_table(table_name(), list()) -> ok.
maybe_create_table(TabName, TabSpec) ->
    case mnesia:create_table(TabName, TabSpec) of
	{atomic, ok} ->
	    ok;
	{aborted, {already_exists, _}} ->
	    error_logger:warning_msg(
	      "Table ~s already exists, ASSUMING its definition is up-to-date!",
	      [TabName])
    end.


-spec fix_schema([node()]) -> ok.
fix_schema(Nodes) ->
    ExistingRamCopies = mnesia:table_info(schema, ram_copies),
    NodesWithRamCopies =
	[X || X <- Nodes, lists:member(X, ExistingRamCopies)],
    [{atomic, ok} = mnesia:change_table_copy_type(schema, Node, disc_copies)
     || Node <- NodesWithRamCopies],
    DiskCopies = mnesia:table_info(schema, disc_copies),
    NodesWithoutSchema =
	[X || X <- Nodes, not lists:member(X, DiskCopies)],
    case NodesWithoutSchema of
	[] ->
	    ok;
	N ->
	    stopped = mnesia:stop(), % take mnesia offline for schema creation
	    ok = mnesia:create_schema(N),
	    ok = mnesia:start() % restart mnesia
    end.


-spec wrap_read(list() | {aborted, atom()}) -> object() | mnesia_error().
wrap_read([]) ->
    {error, not_found};
wrap_read([Record]) ->
    Record;
wrap_read({aborted, Error}) ->
    {error, Error}.


-spec ban(object()) -> {atomic, integer()} | mnesia_error().
ban(Target) when is_record(Target, target) ->
    mnesia:transaction(
      fun() ->
	      AffectedPaths =
		  mnesia:select(paths, [{#path{target_id='$1', _='_'},
					[{'=:=', '$1', Target#target.id}],
					['$_']}]),
	      lists:map(fun(Path) ->
				UpdatedPath = Path#path{is_banned=true},
				mnesia:write(paths, UpdatedPath, write)
			end, AffectedPaths),
	      mnesia:write(targets, Target, write),
	      length(AffectedPaths)
      end).


-spec generate_id(targets | paths) -> id() | {error, unable_to_generate_id}.
generate_id(Table) ->
    generate_id(Table, 0).


-spec generate_id(targets | paths, integer()) ->
			 id() |
			 {error, unable_to_generate_id}.
generate_id(Table, Attempts) when Attempts < 20 ->
    Id = re:replace(
	   base64:encode(
	     crypto:rand_bytes(3)),
	   "[\/+=]", "",
	   [global, {return, binary}]),
    case mnesia:dirty_read(Table, Id) of
	[] -> Id;
	_Record -> generate_id(Table, Attempts+1)
    end;
generate_id(_Table, _Attempts) ->
    {error, unable_to_generate_id}.
