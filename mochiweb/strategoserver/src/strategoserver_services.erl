%% author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

-module(strategoserver_services).
-author("Nick Crafford <nickcrafford@gmail.com>").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("strategoserver.hrl").
-export([createGame/4, checkState/2, getGameBoard/2, setBoard/3, makeMove/6, rematch/1, getLeaderBoard/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check the current state of the game according to the DB against 
% the passed values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkState(GamePlayerId, Checksum) ->
  GamePlayer = db_util:getRecordByKey(game_players, GamePlayerId),
  Game       = db_util:getRecordByKey(games, GamePlayer#game_players.gameId),
  Game#games.checksum =:= Checksum.

saveGameChecksum(GameId) ->
  Game  = db_util:getRecordByKey(games, GameId),
  Str   = integer_to_list(Game#games.lastMove) ++ ":" ++ atom_to_list(Game#games.gameStatus) ++ ":" ++ GameId,
  MD5   = hexstring(erlang:md5(Str)),

  db_util:updateRecord(Game#games{checksum = MD5}),
  ok. 

hexstring(X) ->
  lists:flatten([io_lib:format("~2.16.0b",[N])||N<-binary_to_list(X)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Top 25 leaders based on raw wins
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getLeaderBoard() ->
  Leaderboards = db_util:getRecordsByMatch({leaderboards, '_','_','_'}),

  Leaders = lists:map(
    fun(X) ->
      [{email,   X#leaderboards.emailAddress},
       {alias,   X#leaderboards.alias},
       {numWins, X#leaderboards.numWins}]
    end, Leaderboards),

  [{leaders, Leaders}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Return the current game board. This includes: the game name,
% the player number, the game status, and the piece positions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getGameBoard(GameId, GamePlayerId) ->
  Game          = db_util:getRecordByKey(games,GameId),
  GamePlayer    = db_util:getRecordByKey(game_players,GamePlayerId),
  GamePositions = db_util:getRecordsByMatch({game_boards,'_',GameId,
                                             '_','_','_','_'}),

  % Remove any outstanding messages for the passed game player
  ok = strategoserver_messages:delPlayerMessages(GamePlayerId),

  % Figure out the player number
  if
    GamePlayer#game_players.isPlayer1 =:= 1 ->
      PlayerNumber = 1;
    true ->
      PlayerNumber = 2
  end,

  IsOpposingPlayer1   = GamePlayer#game_players.isPlayer1 bxor 1,
  OpposingGamePlayers = db_util:getRecordsByMatch({game_players,'_',GameId,IsOpposingPlayer1,'_','_'}),

  if
    length(OpposingGamePlayers) > 0 ->
      [OpposingGamePlayer]    = OpposingGamePlayers,
      OpposingGamePlayerAlias = OpposingGamePlayer#game_players.alias;
    true ->
      OpposingGamePlayerAlias = ""
  end,

  % Only include the rank if for the passed GamePlayer and package 
  % up into mochijson friendly tuples
  GameBoard = lists:map(
    fun(X) ->
      if
        X#game_boards.gamePlayerId =:= GamePlayerId ->
          {struct, [{x, X#game_boards.x},
                    {y, X#game_boards.y},
                    {r, X#game_boards.r}]};
        true ->
          {struct, [{x, X#game_boards.x},
                    {y, X#game_boards.y},
                    {r, <<"">>}]}
      end
    end, GamePositions
  ),

  web_util:returnMsg(success,none,getGameBoard,
    [{gameName,                list_to_binary(Game#games.gameName)},
     {gameStatus,              Game#games.gameStatus},
     {playerNumber,            PlayerNumber},
     {gameId,                  list_to_binary(Game#games.gameId)},
     {isPlayersTurn,           GamePlayerId =:= Game#games.gamePlayerIdInTurn},
     {gamePlayerId,            list_to_binary(GamePlayer#game_players.gamePlayerId)},
     {gamePlayerBoardSet,      GamePlayer#game_players.isBoardSet},
     {gamePlayerAlias,         list_to_binary(GamePlayer#game_players.alias)},
     {opposingGamePlayerAlias, list_to_binary(OpposingGamePlayerAlias)},
     {lastMove,                Game#games.lastMove},
     {board,                   GameBoard}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a re-match from the passed game id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rematch(GameId) ->
  [GamePlayer1] = db_util:getRecordsByMatch({game_players,'_',GameId,1,'_', '_'}),
  [GamePlayer2] = db_util:getRecordsByMatch({game_players,'_',GameId,0,'_', '_'}),

  createGame(GamePlayer2#game_players.isPlayer1, 
             GamePlayer2#game_players.emailAddress, 
             GamePlayer1#game_players.emailAddress,
             0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creats a game with the given name.
% Also, assigns the player that created the game
% to is as a GamePlayer. Set the gameStatus to OPEN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
createGame(IsPlayer1, GamePlayerEmail, OpposingPlayerEmail, IsTest) 
when IsPlayer1                  >= 0,
     IsPlayer1                  =< 1,
     length(GamePlayerEmail)     > 0,
     length(OpposingPlayerEmail) > 0 ->

  GameId                  = db_util:uid(),
  GamePlayerId            = db_util:uid(),
  OpposingGamePlayerId    = db_util:uid(),
  GameName                = "Game " ++ db_util:to_hex(crypto:rand_bytes(5)),
  GamePlayerAlias         = emailToAlias(GamePlayerEmail),
  OpposingGamePlayerAlias = emailToAlias(OpposingPlayerEmail),
  TimeStamp               = db_util:timestamp(),
  
  %Store a record for the game
  ok = db_util:insertRecord(#games{gameId              = GameId,
                                   gameName            = GameName,
                                   gameStatus          = full,
                                   gamePlayerIdInTurn  = GamePlayerId,
                                   lastMove            = TimeStamp,
                                   checksum            = "",
                                   winningGamePlayerId = ""}),

  %Store a record for the game player that started the game
  ok = setGamePlayer(GamePlayerId, GameId, IsPlayer1, GamePlayerAlias, GamePlayerEmail),
      
  %Store a record for the opposing game player
  ok = setGamePlayer(OpposingGamePlayerId, GameId, 1 bxor IsPlayer1, OpposingGamePlayerAlias, OpposingPlayerEmail),

  %Store the initial game checksum
  ok = saveGameChecksum(GameId),

  if
    IsTest =:= 1 ->
      %Return a message to the player who created the game
      web_util:returnMsg(success, gameCreated, createGame,
                         [{gamePlayerId,         list_to_binary(GamePlayerId)},
                          {gameId,               list_to_binary(GameId)},
                          {lastMove,             TimeStamp},
                          {opposingGamePlayerId, list_to_binary(OpposingGamePlayerId)}]);
    true ->
      %Send Invite Email to Game Player who initiated the game
      sendInviteEmail(GameId, GamePlayerId, GamePlayerEmail, OpposingPlayerEmail),

      %Send Invite Email to opposing game player
      sendInviteEmail(GameId, OpposingGamePlayerId, OpposingPlayerEmail, GamePlayerEmail),

      web_util:returnMsg(success, gameCreated, createGame, [{lastMove, TimeStamp}])
  end;
createGame(_,_,_,_) ->
  web_util:returnMsg(error, couldNotCreateGame, createGame).

emailToAlias(PlayerEmail) ->
  string:substr(PlayerEmail,1 ,string:chr(PlayerEmail, $@)-1).

sendInviteEmail(GameId,GamePlayerId,GamePlayerEmail, OpposingEmail) ->
  {ok, EmailTemplate} = strategoserver_config:getVal(email_template),
  {ok, EmailSubject}  = strategoserver_config:getVal(email_subject),
  {ok, EmailDomain}   = strategoserver_config:getVal(email_domain),

  GameUrl   = createBoardUrl(EmailDomain, GameId, GamePlayerId),

  EmailBody = web_util:renderFromTemplate(EmailTemplate,
                                          invite_email,
                                          [{board_url,GameUrl},
                                           {email_address, GamePlayerEmail},
                                           {opposing_email_address,OpposingEmail}]),

  email_util:spawn_email(GamePlayerEmail, EmailSubject, EmailBody),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a URL to send to a game player's opponent is applicable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
createBoardUrl(Domain, GameId, GamePlayerId) ->
  "http://"++Domain++"/board/"++GameId++"/"++GamePlayerId++"/".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set a game player for the given game
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setGamePlayer(GamePlayerId, GameId, IsPlayer1, GamePlayerAlias, GamePlayerEmail) ->
  db_util:insertRecord(#game_players{gamePlayerId = GamePlayerId,
                                     gameId       = GameId,
                                     isPlayer1    = IsPlayer1,
                                     isBoardSet   = false,
                                     alias        = GamePlayerAlias,
                                     emailAddress = GamePlayerEmail}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sets the initial board positions for the passed GamePlayer. This data will be 
% returned via the getGameBoard function whenever the game board is pulled from 
% the server.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setBoard(GameId, GamePlayerId, BoardPositions) ->
  % Determine if this GamePlayer has already set their borad. If not then
  % validate the board and set it if possible
  GamePlayer = db_util:getRecordByKey(game_players, GamePlayerId),
  IsPlayer1  = GamePlayer#game_players.isPlayer1 band 1,

  if
    % Board is NOT set
    GamePlayer#game_players.isBoardSet =/= true ->

      % Validate board positions
      ValidBoard = strategoserver_rules:validateStartingBoard(IsPlayer1,
                                                              BoardPositions),
      if 
        ValidBoard =:= true ->
          DList = db_util:getRecordsByMatch({game_boards,'_','_', 
                                             GamePlayerId, '_','_','_'}),
          % Delete existing board positions
          % Insert new board positions
          db_util:deleteAndInsertRecords(DList, BoardPositions, fun(Item) ->
            {struct,[{_, X},{_,Y},{_,R}]} = Item,
            #game_boards{gameBoardId  = db_util:uid(),
                         gameId       = GameId,
                         gamePlayerId = GamePlayerId,
                         x            = X,
                         y            = Y,
                         r            = R}
          end),

          % Update the board set status for the game player
          db_util:updateRecord(GamePlayer#game_players{isBoardSet = true}),
          PlayerBoardStatus = boardSet;
        true ->
          PlayerBoardStatus = invalidBoard
      end;
    true ->
      PlayerBoardStatus = boardAlreadySet
  end,

  % Figure out what messages to send to the player(s) based on the board status
  if 
    % The GamePlayer has a valid board that has just been set. We will either
    % return boardSet or readyForPlay depending on if the opposing player 
    % already has a set board.
    PlayerBoardStatus =:= boardSet ->
      % How many players already have their boards set?
      GamePlayersWithBoardSet = db_util:getRecordsByMatch({game_players,'_',GameId,'_',true, '_'}),

      if
        % This GamePlayer set their board last. Send a message to both players
        % Letting them know that the game is ready to start
        length(GamePlayersWithBoardSet) =:= 2 ->
          setGameStatus(GameId, started),
          OMsg = web_util:returnMsg(success, readyForPlay, setBoard),
          strategoserver_messages:addMessageForOpposingPlayer(GamePlayerId, OMsg);

        % This GamePlayer has set their board first. Send a message to both 
        % players letting them know that the board has been set for this player. 
        % The game is not ready to start.
        true ->
          OMsg = web_util:returnMsg(success, boardSet, setBoard),
          strategoserver_messages:addMessageForOpposingPlayer(GamePlayerId, OMsg)
      end;

    % The board is either invalid or alreadySet
    PlayerBoardStatus =:= boardAlreadySet ->
      OMsg = web_util:returnMsg(success, boardAlreadySet, setBoard);
    true ->
      OMsg = web_util:returnMsg(error, PlayerBoardStatus, setBoard)
  end,
  %Update the game checksum
  saveGameChecksum(GameId),
  OMsg.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the SlotInfo for the passed match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getGameBoardSlotInfo(SlotMatch) when length(SlotMatch) > 0 ->
    [Pos]        = SlotMatch,
    Rank         = Pos#game_boards.r,
    GamePlayerId = Pos#game_boards.gamePlayerId,
    {Pos, Rank, GamePlayerId};
getGameBoardSlotInfo(_) ->
    {[], <<"">>, ""}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Retrieve a single game board slot based on a game id and x/y coords
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getGameBoardSlot(GameId, X, Y) ->
  db_util:getRecordsByMatch({game_boards,'_',GameId,'_',X,Y,'_'}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Retrieve Opposing GamePlayerId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getOpposingGamePlayerId(GamePlayerId, GameId) -> 
  GamePlayer           = db_util:getRecordByKey(game_players,GamePlayerId),
  IsOpposingPlayer1    = GamePlayer#game_players.isPlayer1 bxor 1,
  [OpposingGamePlayer] = db_util:getRecordsByMatch({game_players,'_',GameId,
                                                    IsOpposingPlayer1,'_','_'}),
  OpposingGamePlayer#game_players.gamePlayerId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Remove a slot from the game board. Only occupied slots reside in the DB
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delGameBoardSlot(GameBoardId) ->
  ok = db_util:deleteRecord(game_boards, GameBoardId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set a game board slot. If not game board id is provided then generate one!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setGameBoardSlot(GameId, GamePlayerId, X, Y, R) ->
  setGameBoardSlot(db_util:uid(), GameId, GamePlayerId, X, Y, R).
setGameBoardSlot(GameBoardId, GameId, GamePlayerId, X, Y, R) ->
  ok = db_util:insertRecord(#game_boards{gameBoardId  = GameBoardId,
                                         gameId       = GameId,
                                         gamePlayerId = GamePlayerId,
                                         x            = X,
                                         y            = Y,
                                         r            = R}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set the GamePlayer who's turn it is for the passed Game record
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
toggleGamePlayerTurn(Game, GamePlayerId) ->
  LastMove = db_util:timestamp(),
  db_util:updateRecord(Game#games{gamePlayerIdInTurn = GamePlayerId,
                                  lastMove           = LastMove}),
  LastMove.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set the game status for game belonging to the passed GameId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setGameStatus(GameId, GameStatus) ->
  Game = db_util:getRecordByKey(games, GameId),
  ok = db_util:updateRecord(Game#games{gameStatus = GameStatus}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set the winning game player for the given game
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setWinningGamePlayer(GameId, GamePlayerId) ->
  Game = db_util:getRecordByKey(games, GameId),
  ok = db_util:updateRecord(Game#games{winningGamePlayerId = GamePlayerId}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Make a move
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
makeMove(GameId, GamePlayerId, Sx, Sy, Tx, Ty) ->
  Game            = db_util:getRecordByKey(games,GameId),
  GameStarted     = Game#games.gameStatus         =:= started,
  GamePlayersTurn = Game#games.gamePlayerIdInTurn =:= GamePlayerId,

  % Make sure the game has started
  if 
    GameStarted =:= true ->
      {SrcPos, SrcRank, SrcGamePlayerId} = getGameBoardSlotInfo(getGameBoardSlot(GameId, Sx, Sy)),
      {TarPos, TarRank, TarGamePlayerId} = getGameBoardSlotInfo(getGameBoardSlot(GameId, Tx, Ty)),
      ReturnableTargetRank               = strategoserver_rules:getReturnableTargetRank(SrcRank, TarRank),
      OpposingGamePlayerId               = getOpposingGamePlayerId(GamePlayerId, GameId),
      
      if
        % Confirm that the GamePlayer submitting the move is up for a turn
        GamePlayersTurn =:= true ->

          % Validate move
          BoardPositions = db_util:getRecordsByMatch({game_boards,'_',GameId,'_','_','_','_'}),
          MoveResult     = strategoserver_rules:validateMove(GameId, GamePlayerId, Sx, Sy, 
                                                             SrcRank, SrcGamePlayerId,
                                                             Tx, Ty, TarGamePlayerId,
                                                             BoardPositions),
          if
            % The move is valid 
            MoveResult =:= valid ->
              if
                % Check for a piece interaction
                TarRank =/= <<"">> ->
                  {Action, ActionMessage} = strategoserver_rules:handlePieceInteraction(SrcRank, TarRank);
                true ->
                  {Action, ActionMessage} = {no_piece_interaction, ""}
              end,

              % Delete Src slot.. Implicitly handles the Action "remove_piece"
              delGameBoardSlot(SrcPos#game_boards.gameBoardId),

              if
                % Move the Source piece into a valid blank slot
                Action =:= no_piece_interaction ->
                  setGameBoardSlot(GameId, SrcGamePlayerId, Tx, Ty, SrcRank);

                %  Move the Source piece into the slot of the target piece
                Action =:= advance_piece ->
                  setGameBoardSlot(TarPos#game_boards.gameBoardId, GameId, 
                                   SrcGamePlayerId, Tx, Ty, SrcRank);

                true ->
                  ok
              end,

              % Set the "isTurn" flag to the opposing player
              LastMove = toggleGamePlayerTurn(Game, OpposingGamePlayerId),

              % Send the "Move completed" message to the opposing player
              % Along with coordinates to update their board
              strategoserver_messages:addMessageForOpposingPlayer(GamePlayerId,
                web_util:returnMsg(success,
                          Action,
                          opponentMakeMove,
                          [{sx, Sx}, {sy, Sy}, {sr, <<"">>},
                           {tx, Tx}, {ty, Ty}, {tr, ReturnableTargetRank},
                           {moveMessage, <<"">>},
                           {lastMove, LastMove}])
              ),

              % If this is a winning move then set the gameStatus to "complete"
              WinningMove = strategoserver_rules:isWinningMove(GameId, GamePlayerId, Sx, Sy, 
                                                               SrcRank, SrcGamePlayerId,
                                                               Tx, Ty, TarRank, TarGamePlayerId, 
                                                               Action),

              if 
                WinningMove ->
                  setGameStatus(GameId, completed),
                  setWinningGamePlayer(GameId, GamePlayerId);
                true ->
                  ok
              end,

              % Return a "Move completed" message to the passed player
              OMsg = web_util:returnMsg(success, Action, makeMove,
                                        [{sx, Sx}, {sy, Sy}, {sr, <<"">>},
                                         {tx, Tx}, {ty, Ty}, {tr, ReturnableTargetRank},
                                         {moveMessage, list_to_binary(ActionMessage)},
                                         {lastMove, LastMove}]);

            % The move is invalid 
            true ->
              OMsg = web_util:returnMsg(error, MoveResult, makeMove,[])
          end;

        % It isn't the GamePlayer's turn
        true ->
          OMsg = web_util:returnMsg(error, notPlayersTurn, makeMove,[])
      end;
      
    % The Game hasn't started yet
    true ->
      OMsg = web_util:returnMsg(error, game_not_started, makeMove,[])
  end,
  %Update the game checksum
  saveGameChecksum(GameId),
  OMsg.
