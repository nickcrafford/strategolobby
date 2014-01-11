%% @author Nick Crafford <nick@webdevengines.com>
%% @copyright 2011 Nick Crafford.

% Holds all the recently created games
-record(games, {gameId, gameName, gameStatus, gamePlayerIdInTurn, lastMove, checksum, winningGamePlayerId}).

% Holds all the game/player associations
-record(game_players, {gamePlayerId, gameId, isPlayer1, isBoardSet, alias, emailAddress}).

% Holds each game's current board state
-record(game_boards, {gameBoardId, gameId, gamePlayerId, x, y, r}).

% Holds message for each game player
-record(game_player_messages, {messageId, gamePlayerId, message}).

% Tracks the current HTTP process of each game player
-record(game_player_processes, {gamePlayerId, processId}).


% Tracks the Leaderboard. Read Only Record
-record(leaderboards, {emailAddress, alias, numWins}).
