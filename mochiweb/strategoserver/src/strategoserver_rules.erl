%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

-module(strategoserver_rules).
-author("Nick Crafford <nickcrafford@gmail.com>").
-include_lib("strategoserver.hrl").
-export([validateMove/10, validateStartingBoard/2, handlePieceInteraction/2, isWinningMove/11, getReturnableTargetRank/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% If the SourceBoardPosition does not exists then we are trying to move a blank 
% slot
%
% If the SourceBoardPosition GamePlayerId = the Oppoing GamePlayer then we are 
% trying to move the opposing player's piece.
%
% If the TargetBoardPosition GamePlayerId = the GamePlayer then we are trying 
% to attack one of our own players
%
% Else this is probably a kosher move sans basic validation

% If the TargetBoardPosition is not found then we are moving into an 
% empty slot (Swap slots)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validateMove(GameId, GamePlayerId, Sx, Sy, SrcRank, SrcGamePlayerId,
             Tx, Ty, TarGamePlayerId, BoardPositions) ->
  SrcIsImovablePiece = lists:member(SrcRank, [<<"F">>,<<"B">>]),
  TarIsInWater       = inWater(Tx,Ty),
  IsDiagonalMove     = isDiagonalMove(Sx,Sy,Tx,Ty),
  MultiSlotMove      = SrcRank =/= <<"2">> andalso isMovingMoreThanOncePlace(Sx,Sy,Tx,Ty),
  IsHoppingLake      = isHoppingLake(Sx,Sy,Tx,Ty),
  IsHoppingPlayer    = isHoppingPlayer(GameId, Sx,Sy,Tx,Ty,BoardPositions),
  
  if
    % GamePlayer is attacking their own piece
    SrcGamePlayerId =:= TarGamePlayerId ->
      invalid;
    % GamePlayer is trying to move a blank slot or the other player's piece
    GamePlayerId =/= SrcGamePlayerId ->
      invalid;
    % GamePlayer is trying to move their flag or a Bomb
    SrcIsImovablePiece =:= true ->
      invalid;
    % GamePlayer is moving into water
    TarIsInWater =:= true ->
      invalid;
    % GamePlayer is moving diagonaly
    IsDiagonalMove =:= true ->
      invalid;
    % Source Piece is not a scout but is trying to move more than one place
    MultiSlotMove =:= true ->
      invalid;
    % GamePlayer is trying to hop a lake
    IsHoppingLake =:= true ->
      invalid;
    % GamePlayer is hopping another player
    IsHoppingPlayer =:= true ->
      invalid;
    % Valid move
    true ->
      valid
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Determine which piece source or target to remove based on the two ranks in play
% A message may also be generated if applicable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handlePieceInteraction(SrcRank, TarRank) ->  
  GameOver           = TarRank =:= <<"F">>,
  MinerDisablingBomb = TarRank =:= <<"B">>  andalso SrcRank =:= <<"3">>,
  BombDidJob         = TarRank =:= <<"B">>  andalso SrcRank =/= <<"3">>,
  SpyCaughtMarshal   = TarRank =:= <<"10">> andalso SrcRank =:= <<"S">>,
  SpyCaughtByPlayer  = TarRank =:= <<"S">>,
  SpyDefeated        = SrcRank =:= <<"S">>,
  PlayerScouting     = SrcRank =:= <<"2">>,
    
  if 
    GameOver =:= true ->
      MoveStatus = advance_piece;
    MinerDisablingBomb =:= true ->
      MoveStatus = advance_piece;
    BombDidJob =:= true ->
      MoveStatus = remove_piece;
    SpyCaughtMarshal =:= true ->
      MoveStatus = advance_piece;
    SpyCaughtByPlayer =:= true ->
      MoveStatus = advance_piece;
    SpyDefeated =:= true ->
      MoveStatus = remove_piece;
    true ->
      SrcRankInt = list_to_integer(binary_to_list(SrcRank)),
      TarRankInt = list_to_integer(binary_to_list(TarRank)),
      if
        SrcRankInt >= TarRankInt ->
          MoveStatus = advance_piece;
        true ->
          MoveStatus = remove_piece
      end
  end,
    
  if
    GameOver =:= true ->
      MoveMessage = "Game Over";
    PlayerScouting =:= true ->
      MoveMessage = binary_to_list(TarRank) ++ " Scouted";
    true ->
      MoveMessage = ""
  end,
  
  {MoveStatus, MoveMessage}.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Is the X/Y coordinate in a water square
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inWater(X,Y) ->
  XMatch = (X == 2) or (X == 3) or (X == 6) or (X == 7),
  YMatch = (Y == 4) or (Y == 5),
  XMatch and YMatch.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Is source the X/Y coordinate moving diagonaly 
% to the target X/Y coordinate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isDiagonalMove(Sx, Sy, Tx, Ty) ->
  (abs(Sx - Tx) * abs(Sy - Ty)) /= 0 .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Is the source X/Y moving more than one grid square to
% get to the target X/Y
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isMovingMoreThanOncePlace(Sx, Sy, Tx, Ty) ->
  Abx = abs(Sx - Tx),
  Aby = abs(Sy - Ty),
  ((Abx /= 0) and (Abx /= 1)) or ((Aby /= 0) and (Aby /= 1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Is the source X/Y trying to hop over the lake grid squares
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isHoppingLake(Sx, Sy, Tx, Ty) ->
  ((Sy  > 5) and (Ty < 4) and ((Sx == 6) or (Sx == 7))) or
  ((Sy  < 4) and (Ty > 5) and ((Sx == 6) or (Sx == 7))) or
  ((Sy  > 5) and (Ty < 4) and ((Sx == 6) or (Sx == 7))) or
  ((Sy  < 4) and (Ty > 5) and ((Sx == 6) or (Sx == 7))) or
  ((Sx  < 2) and (Tx > 3) and ((Sy == 4) or (Sy == 5))) or
  ((Sx  > 3) and (Tx < 2) and ((Sy == 4) or (Sy == 5))) or
  ((Sx  < 6) and (Tx > 7) and ((Sy == 4) or (Sy == 5))) or
  ((Sx  > 7) and (Tx < 6) and ((Sy == 4) or (Sy == 5))) .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Is the player hopping another player
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isHoppingPlayer(GameId, Sx, Sy, Tx, Ty, BoardPositions) ->
  % Determine if the move is N, S, E, or W
  MovingNorth = Tx =:= Sx andalso Ty < Sy,
  MovingSouth = Tx =:= Sx andalso Ty > Sy,
  MovingWest  = Ty =:= Sy andalso Tx < Sx,
  MovingEast  = Ty =:= Sy andalso Tx > Sx,

  XPieces = [X || X <- BoardPositions, X#game_boards.x =:= Sx],
  YPieces = [Y || Y <- BoardPositions, Y#game_boards.y =:= Sy],

  % Check the current slots between the target and source move.
  % If any slots are occupied then we have a problem
  if
    MovingNorth =:= true ->
      BadSlots = [X || X
        <- XPieces,
        Ty < X#game_boards.y andalso Sy > X#game_boards.y];
    MovingSouth =:= true ->
      BadSlots = [X || X
        <- XPieces,
        Ty > X#game_boards.y andalso Sy < X#game_boards.y];      
    MovingWest  =:= true ->
      BadSlots = [X || X
        <- YPieces,
        Tx < X#game_boards.x andalso Sx > X#game_boards.x];
    MovingEast  =:= true ->
      BadSlots = [X || X
        <- YPieces,
        Tx > X#game_boards.x andalso Sx < X#game_boards.x];
    true ->
      BadSlots = [0]
  end,
   
  length(BadSlots) > 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Validate that a list of board positions is valid to use as a starting setup
% Positions are expected in the format [{struct,[{x,0},{y,0},{r,10}]}...]
% i.e. mochijson decode format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validateStartingBoard(IsPlayer1, BoardPositions) ->
  validateStartingBoard(IsPlayer1, BoardPositions, length(BoardPositions), 0).
validateStartingBoard(_, [], ListLen, Acc) ->
  Acc =:= ListLen;
validateStartingBoard(IsPlayer1, [H|T], ListLen, Acc) ->
  {struct,[_,{_,Y},_]} = H,
  if 
    Y > 5 andalso IsPlayer1 =:= 1->
      validateStartingBoard(IsPlayer1, T, ListLen, Acc + 1);
    Y < 4 andalso IsPlayer1 =/= 1 ->
      validateStartingBoard(IsPlayer1, T, ListLen, Acc + 1);      
    true ->
      validateStartingBoard(IsPlayer1, T, ListLen, Acc)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check if this move is a winning move
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isWinningMove(GameId, GamePlayerId, Sx, Sy, SrcRank, SrcGamePlayerId,
              Tx, Ty, TarRank, TarGamePlayerId, Action) ->
  TarRank =:= <<"F">> andalso Action =:= advance_piece.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Get the rank to return to the client
% Important for games with limited information available
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getReturnableTargetRank(_, TarRank) when TarRank =:= <<"F">> ->
    TarRank;
getReturnableTargetRank(SrcRank, TarRank) when SrcRank =:= <<"2">> ->
    TarRank;
getReturnableTargetRank(_,_) ->
    <<"">>.
