%% @author eavnvya
%% @doc @todo Add description to node1.


-module(node3).

%% ====================================================================
%% API functions
%% ====================================================================
-export([node/5, stabilize/4, stabilize/1, start/1, start/2, init/2]).

node(Id, Predecessor, Successor, Store, Next) ->	
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor, Store, Next);
		{notify, New} ->
			{Pred,Keep} = notify(New, Id, Predecessor, Store),
			node(Id, Pred, Successor, Keep, Next);
		{request, Peer} ->
			request(Peer, Predecessor, Successor),
			node(Id, Predecessor, Successor, Store, Next);
		{status, Pred, Nx} ->
			{Succ, Nxt} = stabilize(Pred, Id, Successor, Nx),
			node(Id, Predecessor, Succ, Store, Nxt);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor, Store, Next);
		state ->
			io:format("Id: ~w~n", [Id]),
			io:format("Predecessor: ~w~n", [Predecessor]),
			io:format("Successor: ~w~n", [Successor]),
			io:format("Next: ~w~n", [Next]),
			node(Id, Predecessor, Successor, Store, Next);
		stop ->
			io:format("Stopping..."),
			ok;
		probe ->			
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor, Store, Next);
		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor, Store, Next);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor, Store, Next);
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client,
						Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Added, Next);
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Store, Next);
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(Id, Predecessor, Successor, Merged, Next);
		{'DOWN', Ref, process, _ , _} ->
			{Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
			node(Id, Pred, Succ, Store, Nxt);
		_ ->
			io:format("Invalid Msg"),
			node(Id, Predecessor, Successor,Store, Next)
		end.

down(Ref, {_, Ref, _}, Successor, Next) ->
	io:format("Pred died ~n"),
	{nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey,_, Npid}) ->	
	self() ! stabilize,
	Nref = monitor(Npid),
	io:format("Successor died~n"),
	{Predecessor, {Nkey, Nref, Npid}, nil}.


create_probe(Id, {Skey, _ , Spid})->
	io:format("creating probe... Id: ~w~n",[Id]),
	Spid ! {probe, Id, [], erlang:system_time(micro_seconds)}.

remove_probe(T, Nodes) ->
	R = (erlang:system_time(micro_seconds) - T),
	io:format("Probe received back by the sender in ~w and hopped through ~w~n", [R, Nodes]).

forward_probe(Ref, T, Nodes, Id, {Skey, _ , Spid}) ->
	New_Nodes = [Id|Nodes],
	io:format("Forwarding probe at Id: ~w~n",[Id]),
	Spid ! {probe, Ref, New_Nodes, T}.
	
	
stabilize(Pred, Id, Successor, Nx) ->
	{Skey, SRef, Spid} = Successor,
	case Pred of
		nil ->
			Spid ! {notify, {Id, self()}},
			{Successor,Nx};
		{Id, _} ->
			{Successor,Nx};
		{Skey, _} ->
			Spid ! {notify, {Id, self()}},
			{Successor,Nx};
		{Xkey, Xpid} ->			
			case key:between(Xkey, Id, Skey) of
				true ->
					Xpid ! {notify, {Id, self()}},
					drop(SRef),
					Xref = monitor(Xpid),
					self() ! stabilize,
					{{Xkey,Xref, Xpid}, Successor};
				false ->
					Spid ! {notify, {Id, self()}},
					{Successor,Nx}
			end
	end.

schedule_stabilize() ->
	timer:send_interval(1000, self(), stabilize).

stabilize({ _, _, Spid}) ->
	Spid ! {request, self()}.

request(Peer, Predecessor, Next) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil, Next};
		{Pkey, _ , Ppid} ->
			Peer ! {status, {Pkey, Ppid}, Next}
	end.


notify({Nkey, Npid}, Id, Predecessor, Store) ->
	case Predecessor of
		nil ->
			Keep = handover(Id, Store, Nkey, Npid),	
			Nref = monitor(Npid),
			{{Nkey, Nref, Npid}, Keep};
		{Pkey,Pref, Pid} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					Keep = handover(Id, Store, Nkey, Npid),
					drop(Pref),
					NRef = monitor(Npid),
					{{Nkey, NRef , Npid}, Keep};
				false ->
					{Predecessor, Store}
			end
	end.

handover(Id, Store, Nkey, Npid) ->
	{Keep, Rest} = storage:split(Nkey, Id, Store),
	Npid ! {handover, Rest},
	Keep.



start(Id)->
	start(Id, nil).

start(Id, Peer)->
	timer:start(),
	spawn(fun()->init(Id, Peer) end).

init(Id, Peer) ->
	Predecessor = nil,
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	node(Id, Predecessor, Successor, storage:create(), nil).

connect(Id, nil) ->
	{ok, {Id, nil,  self()}};
connect(Id, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			Ref = monitor(Peer),
			{ok, {Skey, Ref, Peer}}
	after 10000 ->
		io:format("Time out: no response~n",[])
end.


add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Client ! {Qref, ok},
			storage:add(Key, Value, Store);
		false ->
			Spid ! {add, Key, Value, Qref, Client},
			Store
end.


lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			{_, Spid} = Successor,
			Spid ! {lookup, Key, Qref, Client}
end.
	

monitor(Pid) ->
	erlang:monitor(process, Pid).
drop(nil) ->
	ok;
drop(Pref) ->
	erlang:demonitor(Pref, [flush]).

%% ====================================================================
%% Internal functions
%% ====================================================================


