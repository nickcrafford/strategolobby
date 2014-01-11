%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

-module(strategoserver_config).
-export([start/0, getVal/1]).
-record(game_config, {key, value}).

start() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(game_config,
                      [{ram_copies, [node()]},
                       {attributes, record_info(fields,game_config)}]),
  {ok, Vals} = file:consult("config.cfg"),

  mnesia:wait_for_tables([game_config],500),

  {atomic, ok} = mnesia:sync_transaction(fun() ->
    loadDb(Vals)
  end).

loadDb([]) ->
  ok;
loadDb([H|T]) ->
  {Key,Value} = H,
  mnesia:write({game_config, Key, Value}),
  loadDb(T).

getVal(Key) ->
  {atomic, [Record]} = mnesia:sync_transaction(fun() ->
      mnesia:read({game_config, Key})
  end),
  {_,_,Value} = Record,
  {ok,Value}.
