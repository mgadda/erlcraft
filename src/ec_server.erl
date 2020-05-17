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
    protocol_version = "1.14.60",
    mcpe_version = "1.14.60",
    player_count = 0,
    max_player_count = 20}}).

start() ->
  register(server, spawn(fun() -> server() end)).

server() ->
  {ok, Socket} = gen_udp:open(19132, [binary]),
  loop(Socket, #server_state{}).

restart() ->
  server ! {stop},
  server().

stop() ->
  server ! {stop}.

loop(Socket, ServerState) ->
  receive
    {udp, Socket, Host, Port, Bin} ->
      io:format("Inbound packet:~n ~p~n", [Bin]),
      case handle_packet(Bin, ServerState) of
        {reply, Reply, NewServerState} ->
          gen_udp:send(Socket, Host, Port, Reply),
          loop(Socket, NewServerState);
        {_, NewServerState} -> loop(Socket, NewServerState)
      end;
    {stop} ->
      gen_udp:close(Socket)
  end.

handle_packet(Bin, ServerState) ->
  Command = raknet:read_command(Bin),
  io:format("~p~n", [Command]),
  {reply, Reply} = execute_command(ServerState#server_state.server_info, Command),
  {reply, Reply, ServerState}.

execute_command(ServerInfo, #command{name = unconnected_ping_open_connections, data = Data}) ->
  Reply = raknet:make_unconnected_pong_open_connections(
    Data#unconnected_ping_open_connections_data.timeSinceStart,
    ServerInfo),
  {reply, Reply}.
