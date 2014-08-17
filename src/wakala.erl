-module(wakala).

%% API
-export([new/0,
         connect/2,
         disconnect/1,
         callback/2,
         send/2
        ]).
-export_type([proxy/0]).

-record(proxy, {
          socket :: port(),
          process :: pid()
         }).
-opaque proxy() :: #proxy{}.
-type callback_message() :: {ok, iolist()} | {error, iolist()}.
-type callback_result() :: ok | error.

%% ===================================================================
%% API functions
%% ===================================================================


%% @doc Create a new wakala() object to be used in the other calls.
-spec new() -> wakala:proxy().
new() ->
    #proxy{}.


%% @doc Connects to the specified {Host, Port} using Proxy.
-spec connect(Proxy :: wakala:proxy(), {Host :: inet:ip_address() | inet:hostname(), Port :: inet:port_number()}) ->
                     wakala:proxy().
connect(Proxy, {Host, Port}) ->
    case gen_tcp:connect(Host, Port, [{active, false}]) of
        {ok, Socket} -> {ok, Proxy#proxy{socket = Socket}};
        Other -> Other
    end.


%% @doc Disconnect the remote host of the Proxy.
-spec disconnect(Proxy :: wakala:proxy()) -> {ok, wakala:proxy()}.
disconnect(#proxy{socket = Socket, process = Pid} = _Proxy) ->
    gen_tcp:close(Socket),
    exit(Pid, ok),
    {ok, #proxy{}}.


%% @doc The Callback to invoke if data is received by Proxy.
-spec callback(Proxy :: wakala:proxy(), Callback :: fun((callback_message()) -> callback_result())) -> wakala:proxy().
callback(#proxy{socket = Socket} = Proxy, Callback) ->
    Pid = spawn(fun() -> recv(Socket, Callback) end),
    Proxy#proxy{
      process = Pid
     }.


%% @doc Send Packet to Proxy.
-spec send(Proxy :: wakala:proxy(), Packet :: iodata()) -> {ok, wakala:proxy()}.
send(#proxy{socket = Socket} = Proxy, Packet) ->
    case gen_tcp:send(Socket, lists:flatten([Packet])) of
        ok -> {ok, Proxy};
        Other  -> Other
    end.


%% ===================================================================
%% Private functions
%% ===================================================================


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
