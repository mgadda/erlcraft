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


%% API
-export([
  read_command/1,
  make_connected_ping_open_connections/1,
  make_unconnected_ping_open_connections/1,
  make_open_connection_request/2,
  parse_open_connection_request/1,
  make_open_connection_reply/2,
  make_open_connection_request_2/5,
  make_open_connection_reply_2/3
]).

%% Definitions based on https://wiki.vg/Pocket_Minecraft_Protocol#Terminology

%% Data Type Bit Widths
-define(BYTE, 8).
-define(SHORT, 16).
-define(INT32, 32).
-define(INT64, 64).
-define(MAGIC, 128).

-record(server_info, {id, name, protocol_version, mcpe_version, player_count, max_player_count}).

-define(OFFLINE_MESSAGE_DATA_ID, 16#00ffff00fefefefefdfdfdfd12345678).
-define(MAGIC_VALUE, ?OFFLINE_MESSAGE_DATA_ID).
-define(MAGIC_BIT_PATTERN, ?MAGIC_VALUE:?MAGIC).


%% Login Packets
-define(ID_CONNECTED_PING_OPEN_CONNECTIONS, 16#01).
make_connected_ping_open_connections(PingId) ->
  <<?ID_CONNECTED_PING_OPEN_CONNECTIONS:?BYTE, PingId:?INT64, ?MAGIC_BIT_PATTERN>>.

-define(ID_UNCONNECTED_PING_OPEN_CONNECTIONS, 16#02).
make_unconnected_ping_open_connections(PingId) ->
  <<?ID_UNCONNECTED_PING_OPEN_CONNECTIONS:?BYTE, PingId:?INT64, ?MAGIC_BIT_PATTERN>>.

-define(ID_OPEN_CONNECTION_REQUEST_1, 16#05).
make_open_connection_request(Version, MTUSize) ->
  <<?ID_OPEN_CONNECTION_REQUEST_1:?BYTE, ?MAGIC_BIT_PATTERN, Version:?BYTE, 0:(MTUSize*8)>>.

read_command(<<?ID_OPEN_CONNECTION_REQUEST_1:?BYTE, ?MAGIC_BIT_PATTERN, Version:?BYTE, _/bytes>>) ->
  { clientOpenConnectionRequest, Version }.

-define(ID_OPEN_CONNECTION_REPLY_1, 16#06).
make_open_connection_reply(ServerId, MTUSize) ->
  <<?ID_OPEN_CONNECTION_REPLY_1:?BYTE, ?MAGIC_BIT_PATTERN, ServerId:?INT64, 0:?BYTE, MTUSize:?SHORT>>.

-define(ID_OPEN_CONNECTION_REQUEST_2, 16#07).
make_open_connection_request_2(Security, Cookie, ServerUDPPort, MTUSize, ClientId) ->
  <<?ID_OPEN_CONNECTION_REQUEST_2:?BYTE,
    ?MAGIC_BIT_PATTERN,
    Security:?BYTE,
    Cookie:?INT64,
    ServerUDPPort:?SHORT,
    MTUSize:?SHORT,
    ClientId:?INT64>>.

-define(ID_OPEN_CONNECTION_REPLY_2, 16#08).
make_open_connection_reply_2(#server_info{id=ServerId}, ClientUDPPort, MTUSize) ->
  <<?ID_OPEN_CONNECTION_REPLY_2:?BYTE,
    ?MAGIC_BIT_PATTERN,
    ServerId:?INT64,
    ClientUDPPort:?SHORT,
    MTUSize:?SHORT,
    0:?BYTE>>.

-define(ID_INCOMPATIBLE_PROTOCOL_VERSION, 16#1A).
make_incompatible_protocol_version(ProtocolVersion, #server_info{id=ServerId}) ->
  <<?ID_INCOMPATIBLE_PROTOCOL_VERSION:?BYTE,
    ProtocolVersion:?BYTE,
    ?MAGIC_BIT_PATTERN,
    ServerId:?INT64>>.

-define(ID_UNCONNECTED_PING_OPEN_CONNECTIONS, 16#1C).
make_unconnected_ping_open_connections(PingId, ServerInfo) ->
  Identifier = lists:concat([
    "MCPE;",
    ServerInfo#server_info.name, ";",
    integer_to_list(ServerInfo#server_info.protocol_version), ";",
    integer_to_list(ServerInfo#server_info.mcpe_version), ";",
    integer_to_list(ServerInfo#server_info.player_count), ";",
    integer_to_list(ServerInfo#server_info.max_player_count)]),

  <<?ID_UNCONNECTED_PING_OPEN_CONNECTIONS:?BYTE,
    PingId:?INT64,
    (ServerInfo#server_info.id):?INT64,
    ?MAGIC_BIT_PATTERN,
    Identifier
    >>.