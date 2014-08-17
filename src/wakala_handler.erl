-module(wakala_handler).
-behaviour(cowboy_websocket_handler).

-export([
         init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).


%%
%% API callbacks
%%

%% @doc Upgrade the protocol to cowboy_websocket.
-spec init({TransportName :: tcp, ProtocolName :: http}, Req :: cowboy_req:req(), Opts :: any()) ->
                  {upgrade, protocol, cowboy_websocket}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


%% @doc Accepts the websocket and initialize a new wakala_proxy using the url's host and port query
%%      parameters. E.g. http://localhost/websocket?host=example.com&port=80
-spec websocket_init(TransportName :: tcp, Req :: cowboy_req:req(), Opts :: any()) ->
                            {ok, Req :: cowboy_req:req(), wakala_proxy:proxy_info()}.
websocket_init(tcp, Req, _Opts) ->
    {Host, Port} = extract_host_and_port(Req),
    self() ! {connect, Host, Port},
    {ok, Req, wakala_proxy:new()}.


%% @doc Handles any information send by the client.
-spec websocket_handle(InFrame :: {text | binary | ping | pong, binary()},
                       Req :: cowboy_req:req(), Proxy :: wakala_proxy:proxy_info()) ->
                              {ok, cowboy_req:req(), wakala_proxy:proxy_info()}.
websocket_handle({text, Msg}, Req, Proxy) ->
    {ok, Proxy} = wakala_proxy:send(Proxy, Msg),
    {ok, Req, Proxy};
websocket_handle(_Data, Req, Proxy) ->
    {ok, Req, Proxy}.


%% @doc Handles any information send by an erlang process.
-spec websocket_info(InFrame :: cowboy_websocket:frame(), Req :: cowboy_req:req(), Proxy :: wakala_proxy:proxy_info()) ->
                            {'ok', cowboy_req:req(), wakala_proxy:proxy_info()} |
                            {'shutdown', cowboy_req:req(), wakala_proxy:proxy_info()} |
                            {'reply', cowboy_websocket:frame(), cowboy_req:req(), wakala_proxy:proxy_info()}.
websocket_info({connect, Host, Port}, Req, Proxy) ->
    Pid = self(),
    {ok, ConnectedProxy} = wakala_proxy:connect(Proxy, {Host, Port}),
    {ok, LinkedProxy} = wakala_proxy:callback(ConnectedProxy, fun(Message) -> Pid ! Message end),
    Response = io_lib:format("Connected to ~s:~b", [Host, Port]),
    {reply, {text, Response}, Req, LinkedProxy};
websocket_info({received, Message}, Req, Proxy) ->
    {reply, {text, Message}, Req, Proxy};
websocket_info({error, _Reason}, Req, Proxy) ->
    {shutdown, Req, Proxy};
websocket_info(_Info, Req, Proxy) ->
    {ok, Req, Proxy}.


%% @doc Closes the websocket.
-spec websocket_terminate(Reason :: any(), Req :: cowboy_req:req(), Proxy :: wakala_proxy:proxy_info()) ->
                                 ok.
websocket_terminate(_Reason, _Req, Proxy) ->
    {ok, _} = wakala_proxy:disconnect(Proxy),
    ok.


%%
%% PRIVATE
%%

%% @doc Extract the host and port from the query string.
-spec extract_host_and_port(Req :: cowboy_req:req()) -> {string(), integer()}.
extract_host_and_port(Req) ->
    {Host, _} = cowboy_req:qs_val(<<"host">>, Req),
    {Port, _} = cowboy_req:qs_val(<<"port">>, Req),
    {binary_to_list(Host), binary_to_integer(Port)}.
