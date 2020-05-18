%%%-------------------------------------------------------------------
%%% @author mgadda
%%% @copyright (C) 2020, Matt Gadda
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2020 20:59
%%%-------------------------------------------------------------------
-module(ec_server).
-author("mgadda").
-include("raknet.hrl").

%% API
-export([start/0, restart/0, stop/0]).

-record(server_state, {
  server_info = #server_info{
    id = 1234,
    name = "ErlCraft",
    protocol_version = 9,
    mcpe_version = "1.14.60",
    player_count = 0,
    max_player_count = 20},
  connections = []}).

-record(session, {address}).

start() ->
  register(server, spawn(fun() -> server() end)).

server() ->
  {ok, Socket} = gen_udp:open(19132, [binary]),
  loop(Socket, #server_state{}).

restart() ->
  server ! {stop},
  server().

stop() ->
  server ! {stop},
  ok.

loop(Socket, ServerState) ->
  receive
    {udp, Socket, Host, Port, Bin} ->
      io:format("Inbound packet: ~p~n", [Bin]),
      Session = #session{address = {inet, {Host, Port}}},
      case handle_packet(Bin, ServerState, Session) of
        {reply, Reply, NewServerState} ->
          gen_udp:send(Socket, Host, Port, Reply),
          loop(Socket, NewServerState);
        {ok, NewServerState} -> loop(Socket, NewServerState)
      end;
    {stop} ->
      gen_udp:close(Socket)
  end.

handle_packet(Bin, ServerState, Session) ->
  Command = raknet:read_command(Bin),
  io:format("Command: ~p~n", [Command]),
  Result = execute_command(ServerState, Command, Session),
  io:format("Reply: ~p~n", [Result]),
  Result.

execute_command(ServerState,
    #command{name=unconnected_ping_open_connections, data=Data}, _) ->
  Reply = raknet:make_unconnected_pong_open_connections(
    Data#unconnected_ping_open_connections_data.timeSinceStart,
    ServerState#server_state.server_info),
  {reply, Reply, ServerState};

execute_command(ServerState,
    #command{name=open_connection_request}, _) ->
  Reply = raknet:make_open_connection_reply(
    ServerState#server_state.server_info#server_info.id,
    ?MTU_SIZE),
  {reply, Reply, ServerState};

execute_command(ServerState,
    #command{name=open_connection_request_2, data=Data}, Session) ->
  io:format("Session: ~p~n", [Session]),
  {inet, {{A, B, C, D}, Port}} = Session#session.address,
  <<Host:32>> = <<A, B, C, D>>,
  MTUSize = Data#open_connection_request_2_data.mtu_size,
  Reply = raknet:make_open_connection_reply_2(
    ServerState#server_state.server_info,
    Host,
    Port,
    MTUSize),
  {reply, Reply, ServerState};

execute_command(ServerState, Command, _) ->
  io:format("Unsupported command: ~p~n", [Command#command.name]),
  {ok, ServerState}.


