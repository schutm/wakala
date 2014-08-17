-module(wakala).

-export([
  new/0,
  connect/2,
  disconnect/1,
  callback/2,
  send/2
]).
-export_type([proxy_info/0]).

-record(proxy_info, {
  socket :: port(),
  process :: pid()
}).
-opaque proxy_info() :: #proxy_info{}.
-type callback_message() :: {ok, iolist()} | {error, iolist()}.
-type callback_result() :: ok | error.

%%
%% API
%%

%% @doc Create a new proxy_info to be used in the other calls.
-spec new() -> proxy_info().
new() ->
  #proxy_info{}.


%% @doc Connects to the specified {Host, Port} using Proxy info.
-spec connect(Proxy :: wakala:proxy_info(),
    {Host :: inet:ip_address() | inet:hostname(), Port :: inet:port_number()}) ->
    {ok, proxy_info()}.
connect(Proxy, {Host, Port}) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [{active, false}]),
  {ok, Proxy#proxy_info{
    socket = Socket
  }}.


%% @doc Disconnect the remote host of the Proxy.
-spec disconnect(Proxy :: wakala:proxy_info()) -> {ok, wakala:proxy_info()}.
disconnect(#proxy_info{socket = Socket, process = Pid} = _Proxy) ->
  gen_tcp:close(Socket),
  exit(Pid, ok),
  {ok, #proxy_info{}}.


%% @doc The Callback to invoke if data is received by Proxy.
-spec callback(Proxy :: wakala:proxy_info(), Callback :: fun((callback_message()) -> callback_result())) -> {ok, wakala:proxy_info()}.
callback(#proxy_info{socket = Socket} = Proxy, Callback) ->
  Pid = spawn(fun() -> recv(Socket, Callback) end),
  {ok, Proxy#proxy_info{
    process = Pid
  }}.


%% @doc Send Packet to Proxy.
-spec send(Proxy :: wakala:proxy_info(), Packet :: iodata()) -> {ok, wakala:proxy_info()}.
send(#proxy_info{socket = Socket} = Proxy, Packet) ->
  ok = gen_tcp:send(Socket, lists:flatten([Packet, 13, 10])),
  {ok, Proxy}.


%%
%% PRIVATE
%%

%% @doc Receive data from the Socket and calls the Callback
-spec recv(Socket :: port(), Callback :: fun((callback_message()) -> callback_result())) -> no_return().
recv(Socket, Callback) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Message} ->
      Callback({received, Message}),
      recv(Socket, Callback);
    {error, Reason} ->
      Callback({error, Reason})
  end.
