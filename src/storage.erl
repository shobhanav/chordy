%% @author eavnvya
%% @doc @todo Add description to storage.


-module(storage).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
	[].

add(Key, Value, Store) ->
	case lists:keyfind(Key, 1, Store) of
		{Key,_} ->
			lists:keyreplace(Key, 1, Store, {Key,Value});			
		false ->
			[{Key, Value}|Store]
	end.

lookup(Key, Store) ->
	case lists:keyfind(Key, 1, Store) of
		{Key,Value} ->
			Value;
		false ->			
			false
	end.

split(From, To, Store) ->
	lists:partition(fun({Key,_})->key:between(Key, From, To) end, Store).

merge(Entries, Store) ->
	lists:keymerge(1, Entries, Store).
	

%% ====================================================================
%% Internal functions
%% ====================================================================


