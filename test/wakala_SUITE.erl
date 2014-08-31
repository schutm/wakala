%% Application level tests. And no, these are not unit tests. Using these test I test the
%% application interface, defined in wakala_handler.

-module(wakala_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%% ct.

all() ->
    [{group, websocket_tests}].

groups() ->
    [{websocket_tests, [parallel], [
                                    websocket_open_failure_test,
                                    websocket_text_test,
                                    websocket_host_closed_connection_test
                                   ]}].

init_per_suite(Config) ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    Config.
init_per_group(Name = websocket_tests, Config) ->
    init_http(Name, [
                     {env, [{dispatch, init_dispatch()}]},
                     {compress, true}
                    ], Config).

end_per_group(Listener, _Config) ->
    cowboy:stop_listener(Listener).

%% Dispatch configuration.

init_dispatch() ->
    cowboy_router:compile([
                           {"localhost", [
                                          {"/wakala", wakala_handler, []}
                                         ]}
                          ]).

%% Tests.
websocket_open_failure_test(Config) ->
    Socket = get_websocket(Config, {"nosuchhost.test", 25}),
    accept_closing_handshake(Socket),

    % The socket should be closed
    {error, closed} = get_response(Socket),
    ok.

websocket_text_test(Config) ->
    Socket = get_websocket(Config, get_smtp_server()),
    "220 " ++ _ = get_response(Socket),

    % The send command should get a response.
    ok = send(Socket, text, "QUIT\r\n"),
    "221 " ++ _ = get_response(Socket),

    close(Socket),
    ok.

websocket_host_closed_connection_test(Config) ->
    Socket = get_websocket(Config, get_smtp_server()),
    "220 " ++ _ = get_response(Socket),

    % Force the host to close the connection
    ok = send(Socket, text, "QUIT\r\n"),
    "221 " ++ _ = get_response(Socket),
    accept_closing_handshake(Socket),

    % We should not be able to read from a closed connection
    {error, closed} = gen_tcp:recv(Socket, 0, 6000),

    % We should not be able to send to a closed connection
    {error, closed} = send(Socket, text, "HELO\r\n"),
    ok.


%% Internal.
get_smtp_server() ->
    {os:getenv("IP"), list_to_integer(os:getenv("SMTP_PORT"))}.

init_http(Ref, ProtoOpts, Config) ->
    {ok, _} = cowboy:start_http(Ref, 100, [{port, 0}], [
                                                        {max_keepalive, 50},
                                                        {timeout, 500}
                                                        |ProtoOpts]),
    Port = ranch:get_port(Ref),
    [{type, tcp}, {port, Port}, {opts, []}|Config].

get_websocket(Config, RemoteHost) ->
    {ok, Socket} = connect(Config),
    upgrade_to_websocket(Socket, RemoteHost),
    Socket.

connect(Config) ->
    Port = ?config(port, Config),
    Options = [binary, {active, false}, {packet, raw}],
    gen_tcp:connect("localhost", Port, Options).

close(Socket) ->
    gen_tcp:close(Socket).

upgrade_to_websocket(Socket, {Host, Port}) ->
    Packet = lists:flatten([
                            "GET /wakala?host=", Host, "&port=", integer_to_list(Port), " HTTP/1.1\r\n"
                            "Host: localhost\r\n"
                            "Connection: Upgrade\r\n"
                            "Upgrade: websocket\r\n"
                            "Sec-WebSocket-Origin: http://localhost\r\n"
                            "Sec-WebSocket-Version: 8\r\n"
                            "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
                            "\r\n"]),
    ok = gen_tcp:send(Socket, Packet),
    accept_opening_handshake(Socket).

accept_opening_handshake(Socket) ->
    {ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
    {ok, HttpResponse, Rest} = erlang:decode_packet(http, Handshake, []),
    [Headers, <<>>] = decode_headers(erlang:decode_packet(httph, Rest, []), []),

    {http_response, {1, 1}, 101, "Switching Protocols"} = HttpResponse,
    {'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
    {'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
    {"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
        = lists:keyfind("sec-websocket-accept", 1, Headers).

accept_closing_handshake(Socket) ->
    Opcode = opcode(close),
    Fin = 1,
    {ok, Data} = gen_tcp:recv(Socket, 0, 6000),
    <<Fin:1, _Reserved:3, Opcode:4, _Mask:1, _Length:7, _Payload/binary>> = Data,
    ok = gen_tcp:send(Socket, << 1:1, 0:3, Opcode:4, 1:1, 0:7, 0:32 >>). %% close

send(Socket, MessageType, Message) ->
    Opcode = opcode(MessageType),
    FrameLength = length(Message),
    Header = <<1:1, 0:3, Opcode:4, 1:1, FrameLength:7>>,
    Mask = 16#37fa213d,
    MaskedMessage = mask(list_to_binary(Message), Mask, <<>>),
    gen_tcp:send(Socket, [Header, <<Mask:32>>, MaskedMessage]).

get_response(Socket) ->
    Response = gen_tcp:recv(Socket, 2, 6000),
    handle_response(Socket, Response).

handle_response(Socket, {ok, Data}) ->
    <<_Fin:1, _Reserved:3, Opcode:4, _Mask:1, Length:7>> = Data,
    {ok, Payload} = gen_tcp:recv(Socket, Length, 0),
    binary_to_list(Payload);
handle_response(_Socket, Response) ->
    Response.

opcode(text) -> 1;
opcode(binary) -> 2;
opcode(close) -> 8.

decode_headers({ok, http_eoh, Rest}, Acc) ->
    [Acc, Rest];
decode_headers({ok, {http_header, _I, Key, _R, Value}, Rest}, Acc) ->
    F = fun(S) when is_atom(S) -> S; (S) -> string:to_lower(S) end,
    decode_headers(erlang:decode_packet(httph, Rest, []),
                   [{F(Key), Value}|Acc]).

mask(<<>>, _, Acc) ->
    Acc;
mask(<< O:32, Rest/bits >>, MaskKey, Acc) ->
    T = O bxor MaskKey,
    mask(Rest, MaskKey, << Acc/binary, T:32 >>);
mask(<< O:24 >>, MaskKey, Acc) ->
    << MaskKey2:24, _:8 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:24 >>;
mask(<< O:16 >>, MaskKey, Acc) ->
    << MaskKey2:16, _:16 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:16 >>;
mask(<< O:8 >>, MaskKey, Acc) ->
    << MaskKey2:8, _:24 >> = << MaskKey:32 >>,
    T = O bxor MaskKey2,
    << Acc/binary, T:8 >>.
