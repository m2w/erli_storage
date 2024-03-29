%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.1a
%%% @doc Data models used throughout the erli application.
%%% Contains the record and type declarations for all models.
%%% @end
%%%==========================================================


%%-----------------------------------------------------------
%% Internal Types
%%-----------------------------------------------------------

-type id() :: bitstring().
-type incremental_id() :: non_neg_integer().
-type unix_timestamp() :: integer().
-type url() :: bitstring().

%%-----------------------------------------------------------
%% Records
%%-----------------------------------------------------------

%% @doc Contains all information on a target URL.
%% This includes links to all related data.
-record(target, {id :: id(),
		 record_number :: incremental_id(),
		 url :: url(),
		 last_modified :: unix_timestamp(),
		 is_banned=false :: boolean(),
		 flag_count=0 :: non_neg_integer(),
		 has_thumbnail=false :: boolean()}).

%% @doc Contains all information on a shortened URL.
%% This includes links to all related data.
-record(path, {id :: id(),
	       record_number :: incremental_id(),
	       target_id :: id(),
	       is_banned=false :: boolean()}).

%% @doc Represents a single visit to a shortened URL.
%% Includes links to all related data.
-record(visit, {id :: id(),
		record_number :: incremental_id(),
		path_id :: id(),
		geo_location :: bitstring(),
		time :: unix_timestamp()}).

%%-----------------------------------------------------------
%% External Types
%%-----------------------------------------------------------

-type range_start() :: pos_integer().
-type range_end() :: pos_integer().
-type range() :: {range_start(), range_end()}.
-type object() :: #target{} | #path{} | #visit{}.
-type collection() :: list(object()).
-type object_type() :: target | path | visit.
-type collection_type() :: targets | paths | visits.
-type relation_type() :: {object_type(), collection_type() | object_type()}.
-type meta_data() :: [{bitstring(), non_neg_integer()}].
-type proplist() :: [{bitstring(), term()}].
-type collection_size() :: non_neg_integer().
-type collection_data() :: {meta_data(), collection()}.
-type conflict_data() :: {new_obj(), existing_obj()}.
-type conflict_error() :: {error, {conflict, conflict_data()}}.
-type mnesia_error() :: {error, atom()}.

%%-----------------------------------------------------------
%% Macros
%%-----------------------------------------------------------

-define(is_collection(X), (X =:= targets orelse X =:= paths orelse X =:= visits)).
-define(is_object(X), (X =:= target orelse X =:= path orelse X =:= visit)).
-define(is_visit_type(X), (X =:= visit orelse X =:= visits)).
