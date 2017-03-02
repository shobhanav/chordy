%% @author eavnvya
%% @doc @todo Add description to key.


-module(key).

%% ====================================================================
%% API functions
%% ====================================================================
-export([generate/0, between/3]).

generate()->
	random:uniform(1000).

between(Key, From, To) when From==To ->
	true;
between(Key, From, To) when From < To->
	if
		(From<Key) and (Key=< To) ->
			true;
		true ->
			false
		end;
between(Key, From, To) when From > To ->
	case between(Key, To, From) of
		true ->
			false;
		false ->
			true
	end.


	   
		 





%% ====================================================================
%% Internal functions
%% ====================================================================


