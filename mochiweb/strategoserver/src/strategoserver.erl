%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

%% @doc TEMPLATE.

-module(strategoserver).
-author('Nick Crafford <nick@webdevengines.com>').
-export([start/1, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the strategoserver server.
start([TPort]) ->
    start([TPort, '']);
start([TPort, TPidFile]) ->
    % Pull port and pidfile to write pid to
    Port    = list_to_integer(atom_to_list(TPort)),
    PidFile = atom_to_list(TPidFile),
    application:set_env(strategoserver, port,     Port),
    application:set_env(strategoserver, pid_file, PidFile),
    strategoserver_deps:ensure(),
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(ssl),
    application:start(strategoserver).

%% @spec stop() -> ok
%% @doc Stop the strategoserver server.
stop() ->
    Res = application:stop(strategoserver),
    application:stop(crypto),
    application:stop(public_key),
    application:stop(ssl),
    Res.
