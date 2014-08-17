-module(wakala_handler).
-behaviour(cowboy_websocket_handler).

%% Cowboy callbacks
-export([init/3]).

%% Cowboy websocket callbacks
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).
-compile(export_all).


%% ===================================================================
%% Cowboy callbacks
%% ===================================================================

%% @doc Upgrade the protocol to cowboy_websocket.
-spec init({TransportName :: tcp, ProtocolName :: http}, Req :: cowboy_req:req(), Opts :: any()) ->
                  {upgrade, protocol, cowboy_websocket}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


%% ===================================================================
%% Cowboy websocket handler callbacks
%% ===================================================================

%% @doc Accepts the websocket and initialize a new wakala_proxy using the url's host and port query
%%      parameters. E.g. http://localhost/websocket?host=example.com&port=80
-spec websocket_init(TransportName :: tcp, Req :: cowboy_req:req(), Opts :: any()) ->
                            {ok, Req :: cowboy_req:req(), wakala:proxy()}.
websocket_init(tcp, Req, _Opts) ->
    self() ! connect,
    {ok, Req, wakala:new()}.

%% @doc Handles any information send by the client.
-spec websocket_handle(InFrame :: {text | binary | ping | pong, binary()},
                       Req :: cowboy_req:req(), Proxy :: wakala:proxy()) ->
                              {ok, cowboy_req:req(), wakala:proxy()}.
websocket_handle({text, Msg}, Req, Proxy) ->
    Result = wakala:send(Proxy, Msg),
    reply(Req, Result).

%% @doc Handles any information send by an erlang process.
-spec websocket_info(InFrame :: cowboy_websocket:frame(), Req :: cowboy_req:req(), Proxy :: wakala:proxy()) ->
                            {'ok', cowboy_req:req(), wakala:proxy()} |
                            {'shutdown', cowboy_req:req(), wakala:proxy()} |
                            {'reply', cowboy_websocket:frame(), cowboy_req:req(), wakala:proxy()}.
websocket_info(connect, Req, Proxy) ->
    RemoteHost = extract_host_and_port(Req),
    Result = connect_host(Proxy, RemoteHost),
    reply(Req, Result);
websocket_info({received, Message}, Req, Proxy) ->
    {reply, {text, Message}, Req, Proxy};
websocket_info({error, _Reason}, Req, Proxy) ->
    {shutdown, Req, Proxy}.

%% @doc Closes the websocket.
-spec websocket_terminate(Reason :: any(), Req :: cowboy_req:req(), Proxy :: wakala:proxy()) ->
                                 ok.
websocket_terminate(_Reason, _Req, Proxy) ->
    {ok, _} = wakala:disconnect(Proxy),
    ok.


%% ===================================================================
%% Private functions
%% ===================================================================

%% @doc Extract the host and port from the query string.
-spec extract_host_and_port(Req :: cowboy_req:req()) -> {string(), integer()}.
extract_host_and_port(Req) ->
    {Host, _} = cowboy_req:qs_val(<<"host">>, Req),
    {Port, _} = cowboy_req:qs_val(<<"port">>, Req),
    {binary_to_list(Host), binary_to_integer(Port)}.

%% @doc Connect to the remote host.
connect_host(Proxy, RemoteHost) ->
    Pid = self(),
    case wakala:connect(Proxy, RemoteHost) of
        {ok, ConnectedProxy} -> {ok, wakala:callback(ConnectedProxy, fun(Message) -> Pid ! Message end)};
        Other -> Other
    end.


-spec reply(Req :: cowboy_req:req(), {ok|error, wakala:proxy()}) ->
                   {ok, cowboy_req:req(), wakala:proxy()}|{shutdown, wakala:proxy()}.
reply(Req, {ok, Proxy}) ->
    {ok, Req, Proxy};
reply(_Req, {error, Proxy}) ->
    {shutdown, Proxy}.
