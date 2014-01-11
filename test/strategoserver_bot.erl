%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

-module(strategoserver_bot).
-include_lib("stdlib/include/qlc.hrl").
-export([start/2,setBoard/4]).

start(ServerString, NumGames) ->
 ok = strategoserver_client:start(),
  createGames(ServerString, NumGames).

% Create N Games
createGames(_,0) ->
  ok;
createGames(ServerString,NumGames) ->
  createGame(ServerString),
  createGames(ServerString,NumGames-1).

% Create a new game and spawn a process to play
createGame(ServerString) ->
  CaptchaVal = "1234876ABCDFE4568DD",
  Data       = strategoserver_client:createGame(
                 ServerString,"Nick@test.com","1","Magnus@test.com", CaptchaVal
               ),
 
  {GameId, GamePlayerId, OpposingGamePlayerId} = Data,

  spawnPlayer(ServerString, 1, GameId, GamePlayerId),
  spawnPlayer(ServerString, 0, GameId, OpposingGamePlayerId),
  timer:sleep(250).

% Spawn a new process to handle the player's playing duties
spawnPlayer(ServerString, IsPlayer1, GameId, GamePlayerId) ->
  spawn(
    fun() ->
      setBoard(ServerString, IsPlayer1, GameId, GamePlayerId),
      getMessages(ServerString, IsPlayer1, GameId, GamePlayerId, 0)
    end
  ).

getMessages(ServerString, IsPlayer1, GameId, GamePlayerId, MvFlag) ->
  
  CallBack = fun() ->
    if
      IsPlayer1 =:= 1 ->
        if
          MvFlag =:= 0 ->
            Sx = 0,
            Sy = 6,
            Tx = 0,
            Ty = 5;
          true ->
            Sx = 0,
            Sy = 5,
            Tx = 0,
            Ty = 6
        end;
      true ->
        if
          MvFlag =:= 0 ->
            Sx = 0,
            Sy = 3,
            Tx = 0,
            Ty = 4;
          true ->
            Sx = 0,
            Sy = 4,
            Tx = 0,
            Ty = 3
        end
    end,

    strategoserver_client:makeMove(ServerString, GameId, GamePlayerId, Sx, Sy, Tx, Ty, fun()->
      if
        MvFlag =:= 0 ->
          NFlag = 1;
        true ->
          NFlag = 0
      end,

      getMessages(ServerString, IsPlayer1, GameId, GamePlayerId, NFlag)
    end)
  end,

  strategoserver_client:getMessages(ServerString, GamePlayerId, CallBack).
  

% Set the board for a gamePlayer
setBoard(ServerString, IsPlayer1, GameId, GamePlayerId) ->
  Ranks = ["3","3","2","2","2","2","2","2","2", "2",
           "B","B","B","B","B","B","S","F","10","9",
           "8","8","7","7","7","6","6","6","6", "5",
           "5","5","5","4","4","4","4","3","3", "3"],
  
  Xs    = [0,1,2,3,4,5,6,7,8,9,
           0,1,2,3,4,5,6,7,8,9,
           0,1,2,3,4,5,6,7,8,9,
           0,1,2,3,4,5,6,7,8,9],

  P1Ys  = [6,6,6,6,6,6,6,6,6,6,
           7,7,7,7,7,7,7,7,7,7,
           8,8,8,8,8,8,8,8,8,8,
           9,9,9,9,9,9,9,9,9,9],

  P2Ys  = [0,0,0,0,0,0,0,0,0,0,
           1,1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2,2,
           3,3,3,3,3,3,3,3,3,3],

  Combine = fun(X,Y,Z) -> "{\"x\":"++integer_to_list(X)++
                          ",\"y\":"++integer_to_list(Y)++
                          ",\"r\":\""++Z++"\"}," end,
  
  if
    IsPlayer1 =:= 0 ->
      Pos = lists:zipwith3(
              Combine, Xs, P2Ys, Ranks
            );
    true ->
     Pos = lists:zipwith3(
             Combine, Xs, P1Ys, Ranks
           )
  end,
  
  Json      = lists:flatten(Pos),
  BoardJson = "["++string:substr(Json,1,length(Json)-1)++"]",
  
  strategoserver_client:setBoard(ServerString, GameId, GamePlayerId, BoardJson),
  ok.
