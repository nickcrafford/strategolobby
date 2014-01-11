from random import shuffle
import urllib, urllib2, os, time, json

class GSClient(object):
    def __init__(self, serverUrl):
        self.serverUrl = serverUrl
        
    def post(self, path, valDict):
        data     = urllib.urlencode(valDict)
        req      = urllib2.Request(self.serverUrl + path, data)
        response = urllib2.urlopen(req)
        data     = response.read()
        return data
        
    def get(self, path):
        response = urllib2.urlopen(self.serverUrl + path)
        return response.read()
        
    def createGame(self, playerNum=1, playerEmail="nick@dude.com",
                   opposingEmail="nick2@dude.com"):
        return json.loads(self.post("/rpc/newGame/",
                                    [("isPlayer1",     playerNum),
                                     ("playerEmail",   playerEmail),
                                     ("opposingEmail", opposingEmail),
                                     ("captchaValue",  "1234876ABCDFE4568DD")]))

    def checkMessages(self, gamePlayerId,checksum):
        return json.loads(self.post("/rpc/getMessages/",
                                    [("gamePlayerId", gamePlayerId),
                                     ("checksum",     checksum)]))
        
    def getGameBoard(self, gameId, gamePlayerId):
        return json.loads(self.post("/rpc/getBoard/", [("gameId",gameId),("gamePlayerId", gamePlayerId)]))

    def setGameBoard(self, gameId, gamePlayerId, player1=True, boardJson=None):
        if not boardJson:
            boardJson = self.generateRandomGameBoard(player1)
        
        post = self.post("/rpc/setBoard/",
                         [("gameId", gameId),
                           ("gamePlayerId", gamePlayerId),
                           ("boardPositions", boardJson)])
        
        return json.loads(post)
        
    def makeMove(self,gameId, gamePlayerId, sx, sy, tx, ty, checksum):
        post = self.post("/rpc/makeMove/",
                         [("gameId",             gameId),
                          ("gamePlayerId",       gamePlayerId),
                          ("sx",                 sx),
                          ("sy",                 sy),
                          ("tx",                 tx),
                          ("ty",                 ty),
                          ("checksum",           checksum)])
        
        return json.loads(post)
        
    def getBoardSlotJson(self, x, y, r):
        return '{"x":'+str(x)+',"y":'+str(y)+',"r":'+'"'+r+'"},'
        
    def generateGameBoard(self, orderedRanks, player1=True):
        boardPos = []
        for y in range(10):
            for x in range(10):
                if (player1 and y > 5) or (not player1 and y < 4):
                    boardPos.append(self.getBoardSlotJson(x,y,orderedRanks.pop()))
        boardJson = ''.join(boardPos)
        return "[" + boardJson[:len(boardJson)-1] + "]"
        
    def generateRandomGameBoard(self,player1=True,frontRow=None):
        rankCounts = {'F' : 1, '10': 1, '9' : 1, '8' : 2,
                      '7' : 3, '6' : 4, '5' : 4, '4' : 4,
                      '3' : 5, '2' : 8, 'S' : 1, 'B' : 6};
        pieces = []
        
        if frontRow:
            for rank in frontRow:
                rankCounts[rank] = rankCounts[rank] - 1
                pieces.append(rank)
        
        for r in rankCounts:
            rc = rankCounts[r]
            for x in range(rc):
                pieces.append(r)
                
        if frontRow:
            pieces.reverse()
        else:
            shuffle(pieces)

        return self.generateGameBoard(pieces,player1)
