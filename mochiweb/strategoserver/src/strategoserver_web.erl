%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

%% @doc Web server for strategoserver.

-module(strategoserver_web).
-author('Nick Crafford <nick@webdevengines.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,

    % Store a pid file
    {ok, PidFile} = application:get_env(strategoserver, pid_file),    

    if
      PidFile =/= "" ->
        write_pid(PidFile);
      true ->
        ok
    end,

    % Load the config info
    strategoserver_config:start(),
    
    % Load the correct DB module
    {ok, Mod}   = strategoserver_config:getVal(db_module),
    {ok, CPath} = strategoserver_config:getVal(ebin_path),
    code:add_path(CPath++"/"++Mod),
    db_util:start(),

    % Start up Mochiweb
    mochiweb_http:start([{max, 100000},{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->  
    mochiweb_http:stop(?MODULE).

loop(Req, _) ->
    strategoserver_http:handle_request(Req).

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

% Write a pid to a file
write_pid(PidFilePath) ->
  {ok, WriteDescr} = file:open(PidFilePath, [raw, write]),
  file:write(WriteDescr,os:getpid()),
  file:close(WriteDescr).
