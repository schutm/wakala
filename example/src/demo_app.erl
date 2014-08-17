-module(demo_app).
-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).

-define(ACCEPTORS, 100).
-define(PORT, 8080).
-define(REFERENCE, wakala_http).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Routes = routes(),
  Dispatch = cowboy_router:compile(Routes),
  Reference = ?REFERENCE,
  Port = ?PORT,
  Acceptors = ?ACCEPTORS,
  TransOpts = [{port, Port}],
  ProtoOpts = [{env, [{dispatch, Dispatch}]}],
  {ok, _} = cowboy:start_http(
    Reference,
    Acceptors,
    TransOpts,
    ProtoOpts),
  demo_sup:start_link().

stop(_State) ->
  ok.


%% ===================================================================
%% Private functions
%% ===================================================================

routes() ->
  [{'_', [
%%		{"/", cowboy_static, {priv_file, handaki, "index.html"}},
%%		{"/websocket", ws_handler, []},
%%		{"/static/[...]", cowboy_static, {priv_dir, handaki, "static"}}
    {"/", cowboy_static, {file, "priv/index.html"}},
    {"/websocket", ws_handler, []},
    {"/static/jquery.min.js", cowboy_static, {file, "priv/static/jquery.min.js"}}
  ]}].
