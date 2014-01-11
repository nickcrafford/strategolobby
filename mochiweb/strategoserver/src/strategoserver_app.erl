%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

%% @doc Callbacks for the strategoserver application.

-module(strategoserver_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for strategoserver.
start(_Type, _StartArgs) ->
    strategoserver_deps:ensure(),
    strategoserver_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for strategoserver.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
