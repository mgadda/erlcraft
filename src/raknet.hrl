%%%-------------------------------------------------------------------
%%% @author mgadda
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2020 15:21
%%%-------------------------------------------------------------------
-author("mgadda").

%% Definitions based on https://wiki.vg/Pocket_Minecraft_Protocol#Terminology

%% Network Configuration
-define(MTU_SIZE, 1440).

%% Data Type Bit Widths
-define(BYTE, 8).
-define(SHORT, 16).
-define(INT32, 32).
-define(INT64, 64).
-define(MAGIC, 128).

-record(server_info, {id, name, protocol_version, mcpe_version, player_count, max_player_count}).
-record(command, {name, data}).

-define(OFFLINE_MESSAGE_DATA_ID, 16#00ffff00fefefefefdfdfdfd12345678).
-define(MAGIC_VALUE, ?OFFLINE_MESSAGE_DATA_ID).
-define(MAGIC_BIT_PATTERN, ?MAGIC_VALUE:?MAGIC).


%% Login Packets
-define(ID_UNCONNECTED_PING_OPEN_CONNECTIONS, 16#01).
-record(unconnected_ping_open_connections_data, {timeSinceStart, clientGUID}).

-define(ID_UNCONNECTED_PONG_OPEN_CONNECTIONS, 16#1C).

-define(ID_OPEN_CONNECTION_REQUEST_1, 16#05).
-record(open_connection_request_data, {protocolVersion}).
-define(ID_OPEN_CONNECTION_REPLY_1, 16#06).

-define(ID_OPEN_CONNECTION_REQUEST_2, 16#07).
-record(open_connection_request_2_data, {inet_address, mtu_size, client_guid}).
-define(ID_OPEN_CONNECTION_REPLY_2, 16#08).

-define(ID_INCOMPATIBLE_PROTOCOL_VERSION, 16#1A).

-define(ID_CONNECTED_PING_OPEN_CONNECTIONS, 16#02).
-record(connected_ping_open_connections_data, {timeSinceStart, tbd}).

