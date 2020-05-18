%%%-------------------------------------------------------------------
%%% @author mgadda
%%% @copyright (C) 2020, Matt Gadda
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2020 07:38
%%%-------------------------------------------------------------------
-module(raknet).
-author("mgadda").
-include("raknet.hrl").

%% API
-export([
  read_command/1,
  make_unconnected_pong_open_connections/2,
  make_open_connection_reply/2,
  make_open_connection_reply_2/4
]).

read_command(
    <<?ID_UNCONNECTED_PING_OPEN_CONNECTIONS:?BYTE,
      TimeSinceStart:?INT64,
      ?MAGIC_BIT_PATTERN,
      ClientGUID:?INT64>>) ->
  #command{
    name = unconnected_ping_open_connections,
    data = #unconnected_ping_open_connections_data{
      timeSinceStart = TimeSinceStart,
      clientGUID = ClientGUID}};

read_command(
    <<?ID_OPEN_CONNECTION_REQUEST_1:?BYTE,
      ?MAGIC_BIT_PATTERN,
      Version:?BYTE,
      _/bytes>>) ->
  #command{
    name = open_connection_request,
    data = #open_connection_request_data{
      protocolVersion = Version}};

read_command(
    <<?ID_OPEN_CONNECTION_REQUEST_2:?BYTE,
      ?MAGIC_BIT_PATTERN,
      IPVersion:?BYTE, Addr:?INT32, Port:?SHORT, %% TODO: support IPv6 here
      MTUSize:?SHORT,
      ClientGUID:?INT64>>) ->
  <<A:?BYTE, B:?BYTE, C:?BYTE, D:?BYTE>> = <<Addr:32>>,
  InetAddress = {inet, {{A, B, C, D}, Port}},
  #command{
    name = open_connection_request_2,
    data = #open_connection_request_2_data{
      inet_address = InetAddress,
      mtu_size = MTUSize,
      client_guid = ClientGUID}};

read_command(
    <<?ID_CONNECTED_PING_OPEN_CONNECTIONS:?BYTE,
      TimeSinceStart:?INT64,
      ?MAGIC_BIT_PATTERN,
      Rest/binary>>) ->
  #command{
    name = connected_ping_open_connections,
    data = #connected_ping_open_connections_data{
      timeSinceStart = TimeSinceStart,
      tbd = Rest}};

read_command(<<CommandByte, _/binary>>) ->
  io:format("Unsupported command: ~p~n", [CommandByte]),
  #command{
    name = unknown,
    data = undefined
  }.



make_unconnected_pong_open_connections(TimeSinceStart, ServerInfo) ->
  Identifier = lists:concat([
    "MCPE;",
    ServerInfo#server_info.name, ";",
    integer_to_list(ServerInfo#server_info.protocol_version), ";",
    ServerInfo#server_info.mcpe_version, ";",
    integer_to_list(ServerInfo#server_info.player_count), ";",
    integer_to_list(ServerInfo#server_info.max_player_count)]),

  [<<?ID_UNCONNECTED_PONG_OPEN_CONNECTIONS:?BYTE,
    TimeSinceStart:?INT64,
    (ServerInfo#server_info.id):?INT64,
    ?MAGIC_BIT_PATTERN,
    (length(Identifier)):16>>,
    Identifier].

make_open_connection_reply(ServerId, MTUSize) ->
  <<?ID_OPEN_CONNECTION_REPLY_1:?BYTE,
    ?MAGIC_BIT_PATTERN,
    ServerId:?INT64,
    0:?BYTE,
    MTUSize:?SHORT>>.

make_open_connection_reply_2(#server_info{id=ServerId}, ClientHost, ClientPort, MTUSize) ->
  <<?ID_OPEN_CONNECTION_REPLY_2:?BYTE,
    ?MAGIC_BIT_PATTERN,
    ServerId:?INT64,
    4:?BYTE, %% IPv4,
    ClientHost:?INT32,
    ClientPort:?SHORT,
    MTUSize:?SHORT,
    0:?BYTE>>.

make_incompatible_protocol_version(ProtocolVersion, #server_info{id=ServerId}) ->
  <<?ID_INCOMPATIBLE_PROTOCOL_VERSION:?BYTE,
    ProtocolVersion:?BYTE,
    ?MAGIC_BIT_PATTERN,
    ServerId:?INT64>>.

