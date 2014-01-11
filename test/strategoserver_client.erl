%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

-module(strategoserver_client).
-export([start/0,createGame/5,setBoard/4,getMessages/3,makeMove/8]).

% Initalize th module
start() ->
  inets:start(),
  httpc:set_options([{max_sessions, 5000}, {pipeline_timeout, 20000}]),
  ok.

% Create a new game. Return the gameId and both gamePlayerIds
createGame(ServerString, YourEmail, IsPlayer1, OpponentEmail, CaptchaValue) ->

  Dat = postReq(ServerString++"/games/new/",
                "isPlayer1="++IsPlayer1++
                "&playerEmail="++YourEmail++
                "&opposingEmail="++OpponentEmail++
                "&captchaValue="++CaptchaValue),

  {struct, 
   [{<<"status">>,              <<"success">>},
    {<<"message">>,             <<"gameCreated">>},
    {<<"type">>,                <<"createGame">>},
    {<<"gamePlayerId">>,        GamePlayerId},
    {<<"gameId">>,              GameId},
    {<<"opposingGamePlayerId">>,OpposingGamePlayerId}]} = Dat,

  {binary_to_list(GameId), 
   binary_to_list(GamePlayerId), 
   binary_to_list(OpposingGamePlayerId)}.

% Set the board for a given game player
setBoard(ServerString, GameId, GamePlayerId, BoardPositionJson) ->
  erlang:display("Board set for: " ++ GamePlayerId),
  PostData = lists:concat(["gameId=", GameId, "&gamePlayerId=", GamePlayerId, 
                           "&boardPositions=", BoardPositionJson]),
  postReq(ServerString++"/games/setBoard/",PostData).

% Get messages for the passed game player
getMessages(ServerString, GamePlayerId, CallBack) ->
  erlang:display("Getting Messages for: " ++ GamePlayerId),
  getReq(ServerString++"/games/messages/"++GamePlayerId++"/123456",CallBack).

% Make a move
makeMove(ServerString, GameId, GamePlayerId, Sx, Sy, Tx, Ty, CallBack) ->
  erlang:display("Making a move for: " ++ GamePlayerId),
  PostData = "gameId="++GameId++"&gamePlayerId="++GamePlayerId++
             "&sx="++integer_to_list(Sx)++"&sy="++integer_to_list(Sy)++
             "&tx="++integer_to_list(Tx)++"&ty="++integer_to_list(Ty),
  postReq(ServerString++"/games/makeMove/", PostData, CallBack).

% Post some data to the server
postReq(Url, Data) ->
  postReq(Url, Data, fun()-> ok end).
postReq(Url,Data, CallBack) ->
  {ok, RequestId}= httpc:request(post, {Url,[],"application/x-www-form-urlencoded",Data},
                                 [],[{sync, false}]),
  
  receive {http, {RequestId, Result}} ->
    {_,_,Content} = Result,
    timer:sleep(500),
    CallBack(),
    Content
  after 120000 ->
    Content = "[]"
  end,

  mochijson2:decode(Content).

getReq(Url) ->
  getReq(Url, fun()-> ok end).
getReq(Url, CallBack) ->
  {ok, RequestId} = httpc:request(get, {Url,[]},[],[{sync,false}]),
  
  receive {http, {RequestId, Result}} ->
    {_,_,Content} = Result,
    timer:sleep(500),
    CallBack(),
    Content
  after 60000 ->
    Content = "[]"
  end,

  mochijson2:decode(Content).
