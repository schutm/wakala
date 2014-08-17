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
            {"/websocket", wakala_handler, []},
            {"/[...]", cowboy_static, {dir, priv_dir(demo)}}
           ]}].

priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            filename:join(Cwd, "priv");
        Priv ->
            Priv
    end.
