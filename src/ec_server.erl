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
-import(raknet).

%% API
-export([start/0]).

start() ->
  spawn(fun() -> server() end).

server() ->
  {ok, Socket} = gen_udp:open(19132, [binary]),
  loop(Socket).

loop(Socket) ->
  receive
    {udp, Socket, Host, Port, Bin} ->
      BinReply = handle_packet(Bin),
      gen_udp:send(Socket, Host, Port, BinReply),
      loop(Socket)
  end.

handle_packet(<<CommandByte:8, Rest/binary>>) ->
  Command = raknet:read_command(CommandByte),
  execute_command(Command).

execute_command(connected_ping)