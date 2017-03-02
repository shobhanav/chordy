%% @author eavnvya
%% @doc @todo Add description to node1.


-module(node1).

%% ====================================================================
%% API functions
%% ====================================================================
-export([node/3, stabilize/3, stabilize/1, start/1, start/2, init/2]).

node(Id, Predecessor, Successor) ->	
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor);
		{notify, New} ->
			Pred = notify(New, Id, Predecessor),
			node(Id, Pred, Successor);
		{request, Peer} ->
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor);
		{status, Pred} ->
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor);
		state ->
			io:format("Id: ~w~n", [Id]),
			io:format("Predecessor: ~w~n", [Predecessor]),
			io:format("Successor: ~w~n", [Successor]),
			node(Id, Predecessor, Successor);
		stop ->
			io:format("Stopping..."),
			ok;
		probe ->			
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor);
		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor);
		_ ->
			io:format("Invalid Msg"),
			node(Id, Predecessor, Successor)
		end.


create_probe(Id, {Skey, Spid})->
	io:format("creating probe... Id: ~w~n",[Id]),
	Spid ! {probe, Id, [], erlang:system_time(micro_seconds)}.

remove_probe(T, Nodes) ->
	R = (erlang:system_time(micro_seconds) - T),
	io:format("Probe received back by the sender in ~w and hopped through ~w~n", [R, Nodes]).

forward_probe(Ref, T, Nodes, Id, {Skey, Spid}) ->
	New_Nodes = [Id|Nodes],
	io:format("Forwarding probe at Id: ~w~n",[Id]),
	Spid ! {probe, Ref, New_Nodes, T}.
	
	
stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil ->
			Spid ! {notify, {Id, self()}},
			Successor;
		{Id, _} ->
			Successor;
		{Skey, _} ->
			Spid ! {notify, {Id, self()}},
			Successor;
		{Xkey, Xpid} ->			
			case key:between(Xkey, Id, Skey) of
				true ->
					Xpid ! {notify, {Id, self()}},
					{Xkey, Xpid};
				false ->
					Spid ! {notify, {Id, self()}},
					Successor
			end
	end.

schedule_stabilize() ->
	timer:send_interval(1000, self(), stabilize).

stabilize({_, Spid}) ->
	Spid ! {request, self()}.

request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
	end.


notify({Nkey, Npid}, Id, Predecessor) ->
	case Predecessor of
		nil ->
			{Nkey, Npid};
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					{Nkey,Npid};
				false ->
					Predecessor
			end
	end.


start(Id)->
	start(Id, nil).

start(Id, Peer)->
	timer:start(),
	spawn(fun()->init(Id, Peer) end).

init(Id, Peer) ->
	Predecessor = nil,
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	node(Id, Predecessor, Successor).

connect(Id, nil) ->
	{ok, {Id, self()}};
connect(Id, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok, {Skey, Peer}}
	after 10000 ->
		io:format("Time out: no response~n",[])
end.



%% ====================================================================
%% Internal functions
%% ====================================================================


