% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

-module(strategoserver_messages).
-author("Nick Crafford <nickcrafford@gmail.com>").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("strategoserver.hrl").
-export([addMessageForOpposingPlayer/2, addMessage/2, getMessages/1,
         setGamePlayerProcessId/2, delPlayerMessages/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set the processId for the given GamePlayerId to allow messages to be
% delivered more quickly
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setGamePlayerProcessId(GamePlayerId, ProcessId) ->
  db_util:insertRecord(#game_player_processes{gamePlayerId = GamePlayerId,
                                              processId    = ProcessId}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add a message to the queue for the given 
% GamePlayerId's opponent.
%
% The message should be in a mochijson friendly forwat i.e. {struct, [plist]}
% if we want to return it as JSON
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addMessageForOpposingPlayer(GamePlayerId, Message) ->
  CurrentGamePlayer   = db_util:getRecordByKey(game_players, GamePlayerId),
  GameId              = CurrentGamePlayer#game_players.gameId,
  IsPlayer1           = CurrentGamePlayer#game_players.isPlayer1 bxor 1,
  OpposingGamePlayers = db_util:getRecordsByMatch({game_players,'_',GameId,
                                                   IsPlayer1,'_','_'}),

  if 
    length(OpposingGamePlayers) =:= 1 ->
      [OpposingGamePlayer] = OpposingGamePlayers,
      addMessage(OpposingGamePlayer#game_players.gamePlayerId, Message);
    true ->
    ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add a mesage to the queue for the given GamePlayerId
% If their their is a process running registered to the GamePlayerId
% then send the message directly to the process. Otherwise save it
% in the DB for later retrieval.
%
% The message should be in a mochijson friendly forwat i.e. {struct, [plist]}
% if we want to return it as JSON
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
addMessage(GamePlayerId, Message) ->
  % Store the message
  db_util:insertRecord(#game_player_messages{messageId    = db_util:uid(), 
                                             gamePlayerId = GamePlayerId,
                                             message      = Message}),
  
  % Grab the process for the current Game Player
  GamePlayerProcesses = db_util:getRecordsByMatch({game_player_processes, GamePlayerId, '_'}),
    
  % Let the Game Player know that they have a message
  if length(GamePlayerProcesses) =:= 1 ->
    [GamePlayerProcess] = GamePlayerProcesses,
    GamePlayerProcess#game_player_processes.processId ! you_have_messages,
    ok;
  true ->
    ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Return all the messages for the given GamePlayerId. 
% The returned messages will be popped from the queue i.e. deleted
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getMessages(GamePlayerId) ->
  RawMessages = getRawMessages(GamePlayerId),
  getMessageBodies(RawMessages).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert a list of game_player_message records
% into a list of game_player_message.messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getMessageBodies(List) ->
  getMessageBodies(List, []).
getMessageBodies([],List) ->
  List;
getMessageBodies([H|T], List) ->
  NList =  List ++ [H#game_player_messages.message],
  getMessageBodies(T,NList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pull the raw game_player_messages from the DB
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getRawMessages(GamePlayerId) ->
  Messages = db_util:getRecordsByMatch({game_player_messages,
                                        '_', GamePlayerId,'_'}),
  db_util:deleteRecords(Messages),
  Messages.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Delete all messages for the passed GamePlayerId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%u
delPlayerMessages(GamePlayerId) ->
  db_util:deleteRecords(db_util:getRecordsByMatch({game_player_messages,'_', GamePlayerId,'_'})),
  ok.
