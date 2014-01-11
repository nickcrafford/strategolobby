import unittest, md5
from random import shuffle
from GSClient import *
import urllib, urllib2, os, time, json

def getValidFrontRowMove(board, player1):
    if player1:
        fy = 6
    else:
        fy = 3

    for slot in board:
        if slot["r"] not in ("B", "F", "") and slot["y"] == fy and slot["x"] not in(2,3,7,6):
            return slot["x"],slot["y"]
            
    return None, None

def getChecksum(lastMove, gameStatus, gameId):
    chkstr = str(lastMove) + ":" + gameStatus + ":" + gameId
    m      = md5.new()
    m.update(chkstr)
    return m.hexdigest()
                    
class GameManagement(unittest.TestCase):
    
    def setUp(self):        
        self.c = GSClient("http://127.0.0.1:9092")
        
    def test_createGame(self):
        # Test creating a game as P1 on the game server
        createGame = self.c.createGame()
        self.failUnlessEqual(createGame["status"],              "success")
        self.failUnlessEqual(createGame["gameId"]               != "", True)
        self.failUnlessEqual(createGame["gamePlayerId"]         != "", True)
        self.failUnlessEqual(createGame["opposingGamePlayerId"] != "", True)
        
        # Test creating a game as P2 on the game server 
        createGame2 = self.c.createGame(0)
        self.failUnlessEqual(createGame2["status"],            "success")
        self.failUnlessEqual(createGame2["gameId"]               != "", True)
        self.failUnlessEqual(createGame2["gamePlayerId"]         != "", True)
        self.failUnlessEqual(createGame2["opposingGamePlayerId"] != "", True)
        
        # Test creating a game with a bogus player number
        createGame3 = self.c.createGame(3)        
        self.failUnlessEqual(createGame3["status"],  "error")
        self.failUnlessEqual(createGame3["message"], "couldNotCreateGame")

    def test_getMessages(self):
        # Create initial game
        createGame          = self.c.createGame()
        player1GamePlayerId = createGame["gamePlayerId"]
        gameId              = createGame["gameId"]
        lastMove            = createGame["lastMove"]
        player2GamePlayerId = createGame["opposingGamePlayerId"]
        self.c.setGameBoard(gameId, player1GamePlayerId, True)
        self.c.setGameBoard(gameId, player2GamePlayerId, False)
 
        # Check Messages
        chksum = getChecksum(lastMove, "started", gameId)

        messages = self.c.checkMessages(player2GamePlayerId, chksum)
        self.failUnlessEqual(messages[0]["status"],  "success")
        self.failUnlessEqual(messages[0]["message"], "boardSet")

        # Test Pulling Message with an invalid state
        messages = self.c.checkMessages(player2GamePlayerId, "123")
        self.failUnlessEqual(messages[0]["status"],  "error")
        self.failUnlessEqual(messages[0]["message"], "outOfSync")
               
        # Test pulling messages for a non-existent game player
        messages2 = self.c.checkMessages("123456","123")
        self.failUnlessEqual(messages2[0]["status"],  "error")
        self.failUnlessEqual(messages2[0]["message"], "invalidGamePlayerId")

    def test_initialGameBoard(self):
        # Test grabbing a game board from a non-existent game and non-existent player
        gameBoard = self.c.getGameBoard("ABC","123")
        self.failUnlessEqual(gameBoard["status"],  "error")
        self.failUnlessEqual(gameBoard["message"], "invalidCriteria")        
        
        # Test grabbing a game board for a game with nothing set
        createGame   = self.c.createGame(1);
        gameId       = createGame["gameId"]
        gamePlayerId = createGame["gamePlayerId"]
        opposingGamePlayerId = createGame["opposingGamePlayerId"]
        
        # Test grabbing a game board after an opponent joins for player 1
        gameBoardP1          = self.c.getGameBoard(gameId,gamePlayerId)
        self.failUnlessEqual(gameBoardP1["status"],       "success")
        self.failUnlessEqual(gameBoardP1["message"],      "none")
        self.failUnlessEqual(gameBoardP1["gamePlayerId"], gamePlayerId)
        self.failUnlessEqual(gameBoardP1["gameId"],       gameId)
        self.failUnlessEqual(gameBoardP1["gameStatus"],   "full")
        self.failUnlessEqual(gameBoardP1["board"],        [])                
        
        # Test grabbing a game board after an opponent joins for player 2
        gameBoardP2          = self.c.getGameBoard(gameId,opposingGamePlayerId)
        self.failUnlessEqual(gameBoardP2["status"],       "success")
        self.failUnlessEqual(gameBoardP2["message"],      "none")
        self.failUnlessEqual(gameBoardP2["gamePlayerId"], opposingGamePlayerId)
        self.failUnlessEqual(gameBoardP2["gameId"],       gameId)
        self.failUnlessEqual(gameBoardP2["gameStatus"],   "full")
        self.failUnlessEqual(gameBoardP2["board"],        [])
    
    def test_setBoard(self):
        #Test the game server API for setting a game board.
        # Test setting a game board for player 1
        createGame   = self.c.createGame(1);
        gameId       = createGame["gameId"]
        gamePlayerId = createGame["gamePlayerId"]
        opposingGamePlayerId = createGame["opposingGamePlayerId"]
        setBoard     = self.c.setGameBoard(gameId, gamePlayerId, True)
        self.failUnlessEqual(setBoard["status"],  "success")
        self.failUnlessEqual(setBoard["message"], "boardSet")
        self.failUnlessEqual(setBoard["type"],    "setBoard")
        
        # Test setting board again...
        setBoardAgain     = self.c.setGameBoard(gameId, gamePlayerId, True)
        self.failUnlessEqual(setBoardAgain["status"],  "success")
        self.failUnlessEqual(setBoardAgain["message"], "boardAlreadySet")
        self.failUnlessEqual(setBoardAgain["type"],    "setBoard")        

        # Test setting a game board for player 2
        setBoardP2           = self.c.setGameBoard(gameId, opposingGamePlayerId, False)
        self.failUnlessEqual(setBoardP2["status"],  "success")
        self.failUnlessEqual(setBoardP2["message"], "readyForPlay")
        self.failUnlessEqual(setBoardP2["type"],    "setBoard")
        
        # Test setting an invalid board (x/y) coords bad P1
        createGameP1      = self.c.createGame(1)
        invGameIdP1       = createGameP1["gameId"]
        invGamePlayerIdP1 = createGameP1["gamePlayerId"]
        setBoardInvP1     = self.c.setGameBoard(invGameIdP1, invGamePlayerIdP1, False)
        self.failUnlessEqual(setBoardInvP1["status"],  "error")
        self.failUnlessEqual(setBoardInvP1["message"], "invalidBoard")
        self.failUnlessEqual(setBoardInvP1["type"],    "setBoard")              

        # Test setting an invalid board (x/y) coords bad P1
        createGameP2      = self.c.createGame(0)
        invGameIdP2       = createGameP2["gameId"]
        invGamePlayerIdP2 = createGameP2["gamePlayerId"]
        setBoardInvP2     = self.c.setGameBoard(invGameIdP2, invGamePlayerIdP2, True)
        self.failUnlessEqual(setBoardInvP2["status"],  "error")
        self.failUnlessEqual(setBoardInvP2["message"], "invalidBoard")
        self.failUnlessEqual(setBoardInvP2["type"],    "setBoard")   
    
    def test_gameBoard(self):
        # Test grabbing a game board without any players boards set i.e. just player 1
        createGameP1   = self.c.createGame(1)
        gameIdP1       = createGameP1["gameId"]
        gamePlayerIdP1 = createGameP1["gamePlayerId"]
        gamePlayerIdP2 = createGameP1["opposingGamePlayerId"]
        boardP1        = self.c.getGameBoard(gameIdP1, gamePlayerIdP1)
        self.failUnlessEqual(boardP1["status"],       "success")
        self.failUnlessEqual(boardP1["message"],      "none")
        self.failUnlessEqual(boardP1["playerNumber"], 1)
        self.failUnlessEqual(boardP1["board"],        []) 
        
        # Test grabbing a game board without any players boards set after player 2 joins
        boardP2        = self.c.getGameBoard(gameIdP1, gamePlayerIdP2)
        self.failUnlessEqual(boardP2["status"],       "success")
        self.failUnlessEqual(boardP2["message"],      "none")
        self.failUnlessEqual(boardP2["playerNumber"], 2)
        self.failUnlessEqual(boardP2["board"],        []) 

        # Test grabbing a game board after setting player 1's board
        setBoard   = self.c.setGameBoard(gameIdP1, gamePlayerIdP1, True)
        boardP1Set = self.c.getGameBoard(gameIdP1, gamePlayerIdP1)
        self.failUnlessEqual(boardP1Set["status"],         "success")
        self.failUnlessEqual(boardP1Set["gameStatus"],     "full")
        self.failUnlessEqual(boardP1Set["playerNumber"],   1)
        self.failUnlessEqual(len(boardP1Set["board"]) > 0, True)
        
        # Make sure player 1 isn't snooping on player 2
        for dict in boardP1Set["board"]:
            if dict["r"] != "":
                self.failUnlessEqual(dict["y"] > 5, True)
            else:
                self.failUnlessEqual(dict["y"] < 4, True)
    
        # Test grabbing a game board after setting player 2's board
        setBoardP2 = self.c.setGameBoard(gameIdP1, gamePlayerIdP2, False)
        boardP2Set = self.c.getGameBoard(gameIdP1, gamePlayerIdP2)
        self.failUnlessEqual(boardP2Set["status"],         "success")
        self.failUnlessEqual(boardP2Set["gameStatus"],     "started")
        self.failUnlessEqual(boardP2Set["playerNumber"],   2)
        self.failUnlessEqual(len(boardP2Set["board"]) > 0, True)
        
        # Make sure player 2 isn't snooping on player 1
        for dict in boardP2Set["board"]:
            if dict["r"] != "":
                self.failUnlessEqual(dict["y"] < 4, True)
            else:
                self.failUnlessEqual(dict["y"] > 5, True)        
    
    def test_makeMove(self):
        # Test moving out of turn
        # Note: Player 1 always has the first move
        
        boardJsonP1    = self.c.generateRandomGameBoard(True,  ["2","10","9","8","3","B","4","5","F","8"])
        boardJsonP2    = self.c.generateRandomGameBoard(False, ["2","10","9","8","3","B","4","5","F","8"])

        game           = self.c.createGame(1)
        gameId         = game["gameId"]
        gamePlayerIdP1 = game["gamePlayerId"]
        gamePlayerIdP2 = game["opposingGamePlayerId"]
        lastMove       = game["lastMove"]        
        setBoardP1     = self.c.setGameBoard(gameId, gamePlayerIdP1, True, boardJsonP1)
        setBoardP2     = self.c.setGameBoard(gameId, gamePlayerIdP2, False, boardJsonP2)
        self.failUnlessEqual(setBoardP2["status"],  "success")
        self.failUnlessEqual(setBoardP2["message"], "readyForPlay")
        self.failUnlessEqual(setBoardP2["type"],    "setBoard")
        boardP1        = self.c.getGameBoard(gameId, gamePlayerIdP1)["board"]
        boardP2        = self.c.getGameBoard(gameId, gamePlayerIdP2)["board"]

        chksum =  getChecksum(lastMove, "started", gameId)
        
        # Test moving blank slot
        invalidMove =  self.c.makeMove(gameId, gamePlayerIdP1, 0, 4, 0, 5, chksum)
        self.failUnlessEqual(invalidMove["status"],   "error")
        self.failUnlessEqual(invalidMove["message"],  "invalid")
        self.failUnlessEqual(invalidMove["type"],     "makeMove")
        
        # Test moving a non moving piece i.e. a lake
        invalidMove =  self.c.makeMove(gameId, gamePlayerIdP1, 2, 4, 2, 5, chksum)
        self.failUnlessEqual(invalidMove["status"],   "error")
        self.failUnlessEqual(invalidMove["message"],  "invalid")
        self.failUnlessEqual(invalidMove["type"],     "makeMove") 
        
        # Test coords out of bounds
        invalidMove =  self.c.makeMove(gameId, gamePlayerIdP1, 10, 10, 20, 20, chksum)
        self.failUnlessEqual(invalidMove["status"],   "error")
        self.failUnlessEqual(invalidMove["message"],  "invalid")
        self.failUnlessEqual(invalidMove["type"],     "makeMove")

        # Test moving out of turn
        invalidMove =  self.c.makeMove(gameId, gamePlayerIdP2, 0, 4, 0, 5, chksum)
        self.failUnlessEqual(invalidMove["status"],   "error")
        self.failUnlessEqual(invalidMove["message"],  "notPlayersTurn")
        self.failUnlessEqual(invalidMove["type"],     "makeMove")
        
        # Test successful move of player 1 into blank slot
        # First find a valid x/y coord in the front row for player 1
        x,y       = getValidFrontRowMove(boardP1, True)
        validMove = self.c.makeMove(gameId, gamePlayerIdP1, x, y, x, y-1, chksum)
        self.failUnlessEqual(validMove["status"],      "success")
        self.failUnlessEqual(validMove["message"],     "no_piece_interaction")
        self.failUnlessEqual(validMove["type"],        "makeMove")
        self.failUnlessEqual(validMove["moveMessage"], "")

        lastMove = validMove["lastMove"]
        chksum   =  getChecksum(lastMove, "started", gameId)
        
        # Test successful move of player 2
        x,y       = getValidFrontRowMove(boardP2, False)
        validMove = self.c.makeMove(gameId, gamePlayerIdP2, x, y, x, y+1, chksum)
        self.failUnlessEqual(validMove["status"],      "success")
        self.failUnlessEqual(validMove["message"],     "no_piece_interaction")
        self.failUnlessEqual(validMove["type"],        "makeMove")
        self.failUnlessEqual(validMove["moveMessage"], "")

    def test_makeAggresiveMove(self):
        boardJsonP1    = self.c.generateRandomGameBoard(True,  ["2","10","9","8","3","B","4","5","F","8"])
        boardJsonP2    = self.c.generateRandomGameBoard(False, ["2","10","9","8","3","B","4","5","F","8"])
        
        game           = self.c.createGame(1)
        gameId         = game["gameId"]
        lastMove       = game["lastMove"]
        gamePlayerIdP1 = game["gamePlayerId"]
        gamePlayerIdP2 = game["opposingGamePlayerId"]        
        setBoardP1     = self.c.setGameBoard(gameId, gamePlayerIdP1, True,  boardJsonP1)
        setBoardP2     = self.c.setGameBoard(gameId, gamePlayerIdP2, False, boardJsonP2)

        chksum   =  getChecksum(lastMove, "started", gameId)

        # Test moving more than one space and !scout
        invalidMove = self.c.makeMove(gameId, gamePlayerIdP1, 1, 6, 1, 4, chksum)
        self.failUnlessEqual(invalidMove["status"],   "error")
        self.failUnlessEqual(invalidMove["message"],  "invalid")
        self.failUnlessEqual(invalidMove["type"],     "makeMove")
        
        # Test moving more than one space and scout
        validMove = self.c.makeMove(gameId, gamePlayerIdP1, 0, 6, 0, 4, chksum)
        self.failUnlessEqual(validMove["status"],      "success")
        self.failUnlessEqual(validMove["message"],     "no_piece_interaction")
        self.failUnlessEqual(validMove["type"],        "makeMove")
        self.failUnlessEqual(validMove["moveMessage"], "")

        lastMove = validMove["lastMove"]
        chksum   =  getChecksum(lastMove, "started", gameId)
        
        # Token P2 Move
        self.c.makeMove(gameId, gamePlayerIdP2, 0,3,0,4, chksum)

        lastMove = validMove["lastMove"]
        chksum   =  getChecksum(lastMove, "started", gameId)
                
        # Test moving diagonally
        invalidMove = self.c.makeMove(gameId, gamePlayerIdP1, 4, 6, 5, 5, chksum)
        self.failUnlessEqual(invalidMove["status"],   "error")
        self.failUnlessEqual(invalidMove["message"],  "invalid")
        self.failUnlessEqual(invalidMove["type"],     "makeMove")
             
        # Test crossing a lake
        invalidMove = self.c.makeMove(gameId, gamePlayerIdP1, 0, 4, 4, 4, chksum)
        self.failUnlessEqual(invalidMove["status"],   "error")
        self.failUnlessEqual(invalidMove["message"],  "invalid")
        self.failUnlessEqual(invalidMove["type"],     "makeMove")        
        
        # Test attacking own player
        
        # Test scout hopping players
        
        # Test attacking a player and losing
        
        # Test attacking a player and winning
        
        # Test disarming bomb
        
        # Test bieng blown up by a bomb
        
        # Test spy kills marshal
        
        # Test spy kills itself
        
        # Test spy killed
        
        # Test found flag
        
        pass

def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(GameManagement))
    return suite

if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=1).run(suite())

