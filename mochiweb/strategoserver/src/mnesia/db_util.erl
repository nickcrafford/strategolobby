%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

-module(db_util).
-include_lib("../strategoserver.hrl").
-export([start/0, execute/1, uid/0, getRecordByKey/2, getRecordsByMatch/1, 
         insertRecord/1, deleteRecord/2, deleteRecords/1, updateRecord/1, insertRecords/2, 
         exists/2, timestamp/0, to_hex/1]).

start() ->
  crypto:start(),
  mnesia:create_schema([node()]),
  mnesia:start(),
  
  % Persistent Tables
  mnesia:create_table(games,
                      [{disc_copies, [node()]},
                       {attributes,  record_info(fields,games)}]),
  mnesia:create_table(game_players,
                      [{disc_copies, [node()]},
                       {attributes,  record_info(fields,game_players)}]),
  mnesia:create_table(game_boards,
                      [{disc_copies, [node()]},
                       {attributes,  record_info(fields, game_boards)}]),
  % Ram Only Tables
  mnesia:create_table(game_player_messages,
                      [{ram_copies, [node()]},
                       {type,       ordered_set},
                       {attributes, record_info(fields,game_player_messages)}]),
  mnesia:create_table(game_player_processes,
                      [{ram_copies, [node()]},
                       {attributes, record_info(fields,game_player_processes)}]).


getRecordByKey(TableName, RowKey) ->
  {atomic, [Record]} = db_util:execute(
    fun() -> mnesia:read({TableName,RowKey}) end
  ),
  Record.

getRecordsByMatch(MatchTuple) ->
  {atomic, Records} = db_util:execute(
    fun() ->
      mnesia:match_object(MatchTuple)
    end
  ),
  Records.

insertRecord(Record) ->
  {atomic,Status} = db_util:execute(
    fun() ->
      mnesia:write(Record)
    end
  ),
  Status.

deleteRecord(TableName, RowKey) ->
  {atomic,Status} = db_util:execute(
    fun() ->
      mnesia:delete({TableName, RowKey})
    end
  ),
  Status.

deleteRecords(List)->
  {atomic,Status} = db_util:execute(
    fun() ->
      lists:foreach(fun(X) -> mnesia:delete_object(X) end, List)
    end
  ),
  Status.

insertRecords(List, Transform)->
  {atomic,Status} = db_util:execute(
    fun() ->
      lists:foreach(fun(X) -> mnesia:write(Transform(X)) end, List)
    end
  ),
  Status.


updateRecord(Record) ->
  {atomic,Status} = db_util:execute(
    fun() ->
      mnesia:write(Record)
    end
  ),
  Status.
  
exists(Table, Key) ->
  {_, Records} = db_util:execute(
    fun() ->
      mnesia:read({Table, Key})
    end
  ),
  length(Records) >= 1.

% Execute a query against Mnesia
execute(Fun) ->
  mnesia:sync_transaction(Fun).

% Generate a UID
% From Couch DB UUID Generator
uid() ->
  Prefix  = io_lib:format("~14.16.0b", [timestamp()]),
  Res     = list_to_binary(Prefix ++ to_hex(crypto:rand_bytes(9))),
  lists:flatten(binary_to_list(Res)).

% Convert a binary number to hex
to_hex([]) ->
  [];
to_hex(Bin) when is_binary(Bin) ->
  to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
  [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

% Convert a number to a digit char
to_digit(N) when N < 10 -> 
  $0 + N;
to_digit(N) -> 
  $a + N-10.
    
% Get a timestamp (in seconds since Jan 1 1970)
timestamp() ->
  Now     = now(),
  Nowish  = calendar:now_to_universal_time(Now),
  Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
  Then    = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  (Nowsecs - Then).
