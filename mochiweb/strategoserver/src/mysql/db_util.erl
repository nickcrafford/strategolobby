%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

-module(db_util).
-include_lib("../strategoserver.hrl").
-export([start/0, uid/0, getRecordByKey/2, getRecordsByMatch/1, 
         insertRecord/1, deleteRecord/2, deleteRecords/1, deleteAndInsertRecords/3,
         updateRecord/1, insertRecords/2, exists/2, timestamp/0, to_hex/1]).

createPool(0,_,_,_,_) ->
  ok;
createPool(Num, Host, User, Pass, Db) ->
  mysql:connect(pool1,Host, undefined, User, Pass, Db, true),
  createPool(Num-1, Host, User, Pass, Db).

start() ->
  {ok, Host} = strategoserver_config:getVal(mysql_host),
  {ok, User} = strategoserver_config:getVal(mysql_user),
  {ok, Pass} = strategoserver_config:getVal(mysql_password),
  {ok, Db}   = strategoserver_config:getVal(mysql_db),
  {ok, Pool} = strategoserver_config:getVal(mysql_pool_size),
  mysql:start_link(pool1,Host,User,Pass,Db),
  ok = createPool(Pool, Host, User, Pass, Db),
  
  % Prepared Statements for DB insert/update
  mysql:prepare(save_game,                 <<"INSERT INTO strategoserver.games (game_id,game_name,game_status,game_player_id_in_turn,last_move,checksum, winning_game_player_id)  VALUES (?,?,?,?,?,?,?)  ON DUPLICATE KEY UPDATE game_id=VALUES(game_id),  game_name=VALUES(game_name),  game_status=VALUES(game_status),  game_player_id_in_turn=VALUES(game_player_id_in_turn), last_move=VALUES(last_move), checksum=VALUES(checksum), winning_game_player_id=VALUES(winning_game_player_id);">>),
  mysql:prepare(save_game_player,          <<"INSERT INTO strategoserver.game_players (game_player_id,game_id,is_player_1,is_board_set,alias, email_address)  VALUES (?,?,?,?,?,?)  ON DUPLICATE KEY UPDATE game_player_id=VALUES(game_player_id),  game_id=VALUES(game_id),  is_player_1=VALUES(is_player_1),  is_board_set=VALUES(is_board_set), alias=VALUES(alias), email_address=VALUES(email_address);">>),
  mysql:prepare(save_game_board,           <<"INSERT INTO strategoserver.game_boards (game_board_id,game_id,game_player_id,x,y,r)  VALUES (?,?,?,?,?,?)  ON DUPLICATE KEY UPDATE game_board_id=VALUES(game_board_id),game_id=VALUES(game_id),game_player_id=VALUES(game_player_id),  x=VALUES(x),  y=VALUES(y),  r=VALUES(r);">>),
  mysql:prepare(save_game_player_message,  <<"INSERT INTO strategoserver.game_player_messages (message_id,game_player_id,message)  VALUES (?,?,?)  ON DUPLICATE KEY UPDATE message_id=VALUES(message_id),game_player_id=VALUES(game_player_id),  message=VALUES(message);">>),
  mysql:prepare(save_game_player_process,  <<"INSERT INTO strategoserver.game_player_processes (game_player_id,process_id)  VALUES (?,?)  ON DUPLICATE KEY UPDATE game_player_id=VALUES(game_player_id), process_id =VALUES(process_id);">>),


  % Prepared Statements for DB delete
  mysql:prepare(del_game,                 <<"delete from strategoserver.games                 where game_id        = ?">>),
  mysql:prepare(del_game_player,          <<"delete from strategoserver.game_players          where game_player_id = ?">>),
  mysql:prepare(del_game_board,           <<"delete from strategoserver.game_boards           where game_board_id  = ?">>),
  mysql:prepare(del_game_player_message,  <<"delete from strategoserver.game_player_messages  where message_id     = ?">>),
  mysql:prepare(del_game_player_process,  <<"delete from strategoserver.game_player_processes where game_player_id = ?">>),

  % Prepared Statements for DB Exists 
  mysql:prepare(exists_game,                <<"select 1 from strategoserver.games                 where game_id        = ?">>),
  mysql:prepare(exists_game_player,         <<"select 1 from strategoserver.game_players          where game_player_id = ?">>),
  mysql:prepare(exists_game_board,          <<"select 1 from strategoserver.game_boards           where game_board_id  = ?">>),
  mysql:prepare(exists_game_player_message, <<"select 1 from strategoserver.game_player_messages  where message_id     = ?">>),
  mysql:prepare(exists_game_player_process, <<"select 1 from strategoserver.game_player_processes where game_player_id = ?">>),

  % Prepared Statements for DB Get
  mysql:prepare(get_game,                 <<"select * from strategoserver.games                 where game_id        = ?">>),
  mysql:prepare(get_game_player,          <<"select * from strategoserver.game_players          where game_player_id = ?">>),
  mysql:prepare(get_game_board,           <<"select * from strategoserver.game_boards           where game_board_id  = ?">>),
  mysql:prepare(get_game_player_message,  <<"select message_id, game_player_id, message from strategoserver.game_player_messages  where message_id     = ?">>),
  mysql:prepare(get_game_player_process,  <<"select * from strategoserver.game_player_processes where game_player_id = ?">>),
  
  % Prepared Statements for DB Lists
  mysql:prepare(list_game_player_processes, <<"select * from strategoserver.game_player_processes where game_player_id = ?">>),
  mysql:prepare(list_game_player_messages,  <<"select message_id, game_player_id, message from strategoserver.game_player_messages  where game_player_id = ? order by time_stamp asc">>),
  mysql:prepare(list_game_boards,           <<"select * from strategoserver.game_boards           where game_id        = ?">>),
  mysql:prepare(list_game_boards_player,    <<"select * from strategoserver.game_boards           where game_player_id = ?">>),
  mysql:prepare(list_game_players_p1,       <<"select * from strategoserver.game_players          where game_id        = ? and is_player_1  = ?">>),
  mysql:prepare(list_game_players,          <<"select * from strategoserver.game_players          where game_id        = ? and is_board_set = ?">>),
  mysql:prepare(list_game_boards_x,         <<"select * from strategoserver.game_boards           where game_id        = ? and x            = ?">>),
  mysql:prepare(list_game_boards_y,         <<"select * from strategoserver.game_boards           where game_id        = ? and y            = ?">>),
  mysql:prepare(list_game_boards_x_y,       <<"select * from strategoserver.game_boards           where game_id        = ? and x            = ? and y = ?">>),

  % Leader Board DB List
  mysql:prepare(list_leaderboards,          <<"SELECT gp.email_address, gp.alias, count(winning_game_player_id) num_wins from strategoserver.games g, strategoserver.game_players gp where gp.game_player_id = g.winning_game_player_id group by gp.alias, gp.email_address order by count(winning_game_player_id) desc">>).


% Return a record matching the passed key
getRecordByKey(games, RowKey) ->
  singleExec(games, get_game, [RowKey]);
getRecordByKey(game_players, RowKey) ->
  singleExec(game_players, get_game_player, [RowKey]);
getRecordByKey(game_boards, RowKey) ->
  singleExec(game_boards, get_game_board, [RowKey]);
getRecordByKey(game_player_messages, RowKey) ->
  singleExec(game_player_messages, get_game_player_message, [RowKey]);
getRecordByKey(game_player_processes, RowKey) ->
  singleExec(game_player_processes, get_game_player_process, [RowKey]).

% Convert MySQL result set into a Record
convertResultToRecord(games, Result) ->
  [GameId,GameName,GameStatus,GamePlayerIdInTurn,LastMove,Checksum,WinningGamePlayerId] = Result,
  {games,
   binary_to_list(GameId),
   binary_to_list(GameName),
   list_to_atom(binary_to_list(GameStatus)),
   binary_to_list(GamePlayerIdInTurn),
   LastMove,
   binary_to_list(Checksum),
   binary_to_list(WinningGamePlayerId)};
convertResultToRecord(game_players, Result) ->
  [GamePlayerId,GameId, IsPlayer1, IsBoardSet, Alias, EmailAddress] = Result,
  {game_players,
   binary_to_list(GamePlayerId),
   binary_to_list(GameId),
   IsPlayer1,
   list_to_atom(binary_to_list(IsBoardSet)),
   binary_to_list(Alias),
   binary_to_list(EmailAddress)};
convertResultToRecord(game_boards, Result) ->
  [GameBoardId, GameId, GamePlayerId, X, Y, R] = Result,
  {game_boards,
   binary_to_list(GameBoardId),
   binary_to_list(GameId),
   binary_to_list(GamePlayerId),
   X,
   Y,
   R};
convertResultToRecord(game_player_messages, Result) ->
  [MessageId, GamePlayerId, Message] = Result,
  {game_player_messages,
   binary_to_list(MessageId),
   binary_to_list(GamePlayerId),
   binary_to_term(Message)};
convertResultToRecord(game_player_processes, Result) ->
  [GamePlayerId, ProcessId] = Result,
  {game_player_processes,
   binary_to_list(GamePlayerId),
   binary_to_term(ProcessId)};
convertResultToRecord(leaderboards, Result) ->
  [EmailAddress, Alias, NumWins] = Result,
  {leaderboards,
   binary_to_list(EmailAddress),
   binary_to_list(Alias),
   NumWins}.

% Execute a query that will return a single value
singleExec(Type, Stmt, Vals) ->
  {data,{mysql_result,_,[Result],_,_}} =  mysql:execute(pool1, Stmt, Vals),
  convertResultToRecord(Type,Result).

% Execute a query that will return multiple values
multiExec(Type, Stmt, Vals) ->
  Res = mysql:execute(pool1, Stmt, Vals),
  {data,{mysql_result,_,Results,_,_}} = Res,
  [convertResultToRecord(Type, X) || X <- Results].
  
% Return a list of records matching the passed pattern
getRecordsByMatch({game_player_processes, GamePlayerId, '_'}) ->
  multiExec(game_player_processes, list_game_player_processes, [GamePlayerId]);
getRecordsByMatch({game_player_messages,'_', GamePlayerId,'_'}) ->
  multiExec(game_player_messages, list_game_player_messages, [GamePlayerId]);
getRecordsByMatch({game_boards,'_',GameId,'_',Sx,'_','_'}) when Sx =/= '_' ->
  multiExec(game_boards, list_game_boards_x, [GameId, Sx]);
getRecordsByMatch({game_boards,'_',GameId,'_','_',Sy,'_'}) when Sy =/= '_' ->
  multiExec(game_boards, list_game_boards_y, [GameId, Sy]);
getRecordsByMatch({game_boards,'_',GameId,'_','_','_','_'}) ->
  multiExec(game_boards, list_game_boards, [GameId]);
getRecordsByMatch({game_boards,'_','_', GamePlayerId, '_','_','_'}) ->
  multiExec(game_boards, list_game_boards_player, [GamePlayerId]);
getRecordsByMatch({game_boards,'_',GameId,'_',X,Y,'_'}) ->
  multiExec(game_boards, list_game_boards_x_y, [GameId, X, Y]);
getRecordsByMatch({game_players,'_',GameId,IsPlayer1,'_','_'}) ->
  multiExec(game_players, list_game_players_p1, [GameId, IsPlayer1]);
getRecordsByMatch({game_players,'_',GameId,'_',true, '_'}) ->
  multiExec(game_players, list_game_players, [GameId,true]);
getRecordsByMatch({leaderboards,'_','_','_'}) ->
  multiExec(leaderboards, list_leaderboards, []).

% Insert a full record
insertRecord(games,Data) ->
  mysql:execute(pool1, save_game, Data),
  ok;
insertRecord(game_players,Data) ->
  mysql:execute(pool1, save_game_player, Data),
  ok;
insertRecord(game_boards, Data) ->
  mysql:execute(pool1, save_game_board, Data),
  ok;
insertRecord(game_player_messages, Data) ->
  [MessageId, GamePlayerId, Message] = Data,
  RData = [MessageId, GamePlayerId, term_to_binary(Message)],
  mysql:execute(pool1, save_game_player_message, RData),
  ok;
insertRecord(game_player_processes, Data) ->
  [GamePlayerId, Pid] = Data,
  RData = [GamePlayerId, term_to_binary(Pid)],
  mysql:execute(pool1,save_game_player_process, RData),
  ok.
insertRecord(Record) ->
  [RecordType|Data] = tuple_to_list(Record),
  insertRecord(RecordType, Data).

% Delete a record by key
deleteRecord(games,Key) ->
  mysql:execute(pool1, del_game, [Key]),
  ok;
deleteRecord(game_players, Key) ->
  mysql:execute(pool1, del_game_player, [Key]),
  ok;
deleteRecord(game_boards, Key) ->
  mysql:execute(pool1, del_game_board, [Key]),
  ok;
deleteRecord(game_player_messages, Key) ->
  mysql:execute(pool1, del_game_player_message, [Key]),
  ok;
deleteRecord(game_player_processes, Key) ->
  mysql:execute(pool1, del_game_player_process, [Key]),
  ok.

% Delete a list of records
deleteRecords(List)->
  mysql:transaction(
    pool1,
    fun() -> 
      lists:foreach(fun(X) -> 
        [RecordType|T] = tuple_to_list(X),
        [Key|_]        = T, 
        deleteRecord(RecordType, Key) 
      end, List)
    end
  ),
  ok.

% Insert a list of records
insertRecords(List, Transform)->
  mysql:transaction(
    pool1,
    fun() ->
      lists:foreach(fun(X) -> insertRecord(Transform(X)) end, List)
    end
  ),
  ok.

% Delete a list of records and then insert a list of records in a single transaction
deleteAndInsertRecords(DList, IList, ITransform) ->
  mysql:transaction(
    pool1,
    fun() -> 
      % Delete
      lists:foreach(fun(X) -> 
        [RecordType|T] = tuple_to_list(X),
        [Key|_]        = T, 
        deleteRecord(RecordType, Key) 
      end, DList),
      % Insert
      lists:foreach(fun(X) -> insertRecord(ITransform(X)) end, IList)
    end
  ),
  ok.

% Update a record
updateRecord(games, Data) ->
  mysql:execute(pool1, save_game, Data),
  ok;
updateRecord(game_players, Data) ->
  mysql:execute(pool1, save_game_player, Data),
  ok;
updateRecord(game_boards, Data) ->
  mysql:execute(pool1, save_game_board, Data),
  ok;
updateRecord(game_player_messages, Data) ->
  mysql:execute(pool1, save_game_player_message, Data),
  ok;
updateRecord(game_player_processes, Data) ->
  mysql:execute(pool1, save_game_player_process, Data),
  ok.
updateRecord(Record) ->
  [RecordType|Data] = tuple_to_list(Record),
  updateRecord(RecordType,Data).
 
% Check if a record exists 
exists(games, Key) ->
  execExists(exists_game, Key);
exists(game_players, Key) ->
  execExists(exists_game_player, Key);
exists(game_boards, Key) ->
  execExists(exists_game_board, Key);
exists(game_player_messages, Key) ->
  execExists(exists_game_player_message, Key);
exists(game_player_processes, Key) ->
  execExists(exists_game_process, Key).

% Check if a record of any type exists
execExists(Stmt,Key) ->
  {data,{mysql_result,_,Results,_,_}} =  mysql:execute(pool1, Stmt, [Key]),
  length(Results) > 0.

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
