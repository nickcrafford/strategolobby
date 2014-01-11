/******************************************************************************/
/************************ Utility methods. ************************************/
/******************************************************************************/
var util = {

  timeouts : [],

  /* Forward the user to the passed page. */
  jsForward: function(relUrl) {
    top.location = relUrl;
  },
  
  /* Does an element exist on the page? */
  exists: function(obj) {
    return obj.length > 0;
  },

  /* Get a query string paramater value via its name. */
  getParameterByName: function(name) { 
    var match = RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search);
    return match ? decodeURIComponent(match[1].replace(/\+/g, ' ')) : "";
  },

  /* Return a random number from 0-maxNum. */
  getRandomNumber: function(maxNum) {
    return Math.floor(Math.random()*maxNum);
  },
  
  setTimeout: function(fun, millis) {
    var x = setTimeout(fun, millis);
    util.timeouts.push(x);
  },

  clearTimeouts: function() {
    for(var t=0; t < util.timeouts.length; t++) {
      clearTimeout(util.timeouts[t]);
    }
  },

  refreshPage : function() {
    util.clearTimeouts();
    $(".slot").remove();
    //Load the game board if we are on the board page
    if(strategolobby.isBoardPage()) {
      state.initialize(function() {
        ui.initialize(function() {
          strategolobby.initialize();
        });
      });       
    }
  }
}

/******************************************************************************/
/**************** Handles interacting with the server *************************/
/******************************************************************************/
var client = {

  send : function(method, url, data, successFun, errorFun) {
    if(!errorFun) {
      errorFun = function(resp) { 
        alert("Sorry, an error has occurred"); 
        util.refreshPage();
      }
    }
    $.ajax({
      type:    method,
      url:     url,
      data:    data,
      async:   true,
      success: function(json) {
        try {
          var obj = $.parseJSON(json);
          successFun(obj);
        } catch(e) {
          errorFun(json);
        }
      },
      error: function(xhr,textStatus, errorThrown) { 
        errorFun(textStatus); 
      },
    });   
  },
  
  rematch : function(gameId, success, error) {
    client.send("POST", "/rpc/rematch", "gameId="+gameId, success, error);
  },

  createGame : function(isPlayer1, playerEmail, opposingEmail, 
                        captchaValue, success, error) {
    client.send("POST", "/rpc/newGame", 
                "isPlayer1="      + (isPlayer1 ? 1 : 0) +
                "&playerEmail="   + playerEmail +
                "&opposingEmail=" + opposingEmail +
                "&captchaValue="  + captchaValue, success, error);
  },

  getGameBoard : function(gameId, gamePlayerId, success, error) {
    client.send("POST", "/rpc/getBoard", 
                "gameId="+gameId+"&gamePlayerId="+gamePlayerId, success, error);
  },

  checkMessages : function(gamePlayerId, success, error) {
   
   var stateCheck = state.getStateCheckSum();

    client.send("POST", "/rpc/getMessages/",
                "gamePlayerId="        + gamePlayerId+
                "&checksum="           + stateCheck,
                success, error);
  },

  setGameBoard : function(gameId, gamePlayerId, boardJson, success) {
    client.send("POST", "/rpc/setBoard", 
                "gameId="+gameId+"&gamePlayerId="+gamePlayerId+
                "&boardPositions="+boardJson,
                success);
  },

  makeMove : function(gameId, gamePlayerId, sx, sy, tx, ty, success) {

    var stateCheck = state.getStateCheckSum();

    client.send("POST", "/rpc/makeMove", 
                "gameId="+gameId+"&gamePlayerId="+gamePlayerId+
                "&sx="+sx+"&sy="+sy+"&tx="+tx+"&ty="+ty+"&checksum="+stateCheck, success);
  }
}

/******************************************************************************/
/********************* Manages the state of the game. *************************/
/******************************************************************************/
var state = {

  gameId:                  "",
  gamePlayerId:            "",
  gameName:                "",
  gameStatus:              "",
  playerNumber:            "",
  boardSet:                false,
  isPlayer1:               false,
  lastSlotClicked:         "",
  gamePlayerBoardSet:      false,
  boardPositions:          [],
  gamePlayerAlias:         "",
  opposingGamePlayerAlias: "",
  isPlayersTurn:           false,
  lastMoveFromX:           "",
  lastMoveFromY:           "",
  lastMoveToX:             "",
  lastMoveToY:             "",

  initialize : function(callBack) {
    var gameChunks   = window.location.pathname.split("/");
    var gameId       = gameChunks[2];
    var gamePlayerId = gameChunks[3];
    
    client.getGameBoard(gameId, gamePlayerId, 
      function(obj) {
        //Retrieve game state from the server results
        state.gameStatus              = obj.gameStatus;
        state.gameId                  = obj.gameId
        state.gamePlayerId            = obj.gamePlayerId;
        state.gameName                = obj.gameName;
        state.playerNumber            = obj.playerNumber;
        state.isPlayer1               = obj.playerNumber == 1;
        state.boardPositions          = obj.board;
        state.gamePlayerBoardSet      = obj.gamePlayerBoardSet;
        state.boardSet                = obj.gameStatus == "started" || 
                                        obj.gamePlayerBoardSet;
        state.gamePlayerAlias         = obj.gamePlayerAlias;
        state.opposingGamePlayerAlias = obj.opposingGamePlayerAlias;
        state.isPlayersTurn           = obj.isPlayersTurn;
        state.lastMove                = obj.lastMove;

        callBack();
      },
      function(msg) {
        //Game board failed to load
        top.location = '/404/';
      }
    );
  },

  getStateCheckSum : function () {
    var str = state.lastMove + ":" + state.gameStatus + ":" + state.gameId;
    return MD5(str);
  }
}

/******************************************************************************/
/*********************** Manages the UI of the game. **************************/
/******************************************************************************/
var ui = {
  //Constants
  maxMessages             : 5,
  redPieceColor           : "#EB0000",
  bluePieceColor          : "#2B00E1",
  //Static references to DOM objects that will be used a bunch
  gameName                : "",
  playerTag               : "",
  opposingPlayerTag       : "",
  playerToggle            : "",
  opposingPlayerToggle    : "",
  vs                      : "",
  gameBoard               : "",
  gameOverMessage         : "",
  waitingForPlayerMessage : "",
  boardSetWaitingMessage  : "",
  setBoardMessage         : "",
  messageList             : "",
  //Stored values that are useful in more than one place
  playerColor             : "", 
  opposingPlayerColor     : "", 

  /* Initialize the game UI. */
  initialize : function(callBack) {
    //Set DOM elements
    ui.gameName                 = $("#gameName");
    ui.player2Toggle            = $("#player2Toggle");
    ui.player1Toggle            = $("#player1Toggle");
    ui.vs                       = $("#vs");
    ui.playerTag                = $("#playerTag");
    ui.opposingPlayerTag        = $("#opposingPlayerTag");
    ui.playerToggle             = $("#playerToggle");
    ui.opposingPlayerToggle     = $("#opposingPlayerToggle");
    ui.gameBoard                = $("#gameBoard");
    ui.gameOverMessage          = $("#gameOverMessage");
    ui.waitingForPlayerMessage  = $("#waitingForPlayerMessage");
    ui.boardSetWaitingMessage   = $("#boardSetWaitingMessage");
    ui.setBoardMessage          = $("#setBoardMessage");
    ui.gameOnMessage            = $("#gameOnMessage");
    ui.messageList              = $("#messageList");

    //Set Common vals
    ui.playerColor              = state.isPlayer1 ? 
                                  ui.bluePieceColor : ui.redPieceColor;
    ui.opposingPlayerColor      = state.isPlayer1 ? 
                                  ui.redPieceColor  : ui.bluePieceColor; 

    //Setup the board and other UI elements
    ui.renderBoard();
    ui.setGameName();
    ui.setPlayers();
    ui.setMoveFlag(state.isPlayersTurn);
      
    //Display the appropriate board overlay depending on game state
    if(state.gameStatus == "completed") {
      ui.showGameOverMessage();
    } else if(state.gameStatus == "full") {
      if(state.gamePlayerBoardSet) {
        ui.showBoardSetWaiting();
      } else {
        ui.showSetBoard();
      }
    }

    //Add click handler to the game board slots
    $(".slot").click(function() {
      ui.handleSlotClick($(this));
    });
    
    callBack();
  },

  /* Highlight the last two x/y positions moved on the board. */
  hightlightLastMove: function() {
    var slotA = ui.getSlotByCoord(state.lastMoveFromX, state.lastMoveFromY);
    var slotB = ui.getSlotByCoord(state.lastMoveToX,   state.lastMoveToY);
    
    if(slotA && slotB) {
      slotA.effect("highlight", {}, 500);
      slotB.effect("highlight", {}, 500);
    }
  },

  /* Set the name of the current game. */
  setGameName: function() {
    ui.gameName.html(state.gameName);
  },
  
  /* Toggle whether or no it is the player's turn to move. */
  toggleMoveFlag: function() {
    ui.setMoveFlag(!state.isPlayersTurn);
  },
  
  /* Set whether or no it is the player's turn to move. */  
  setMoveFlag: function(flagVal) {
    //Toggle the state
    state.isPlayersTurn = flagVal;
    //Move the toggle
    if(state.isPlayersTurn) {
      ui.opposingPlayerToggle.hide();
      ui.playerToggle.fadeIn();
    } else {
      ui.playerToggle.hide();
      ui.opposingPlayerToggle.fadeIn();
    }     
  },
  
  /* Set the "vs" text with the appropriate player names. */
  setPlayers : function() {
    //If the opposing player has not joined yet then display ???
    var opposingGamePlayerAlias;
    if(state.opposingGamePlayerAlias == "") {
      opposingGamePlayerAlias = "???";
    } else {
      opposingGamePlayerAlias = state.opposingGamePlayerAlias;
    }
 
    //Set colors for the player tags and toggles
    ui.playerTag.css("background-color",            ui.playerColor);
    ui.playerToggle.css("background-color",         ui.playerColor);
    ui.opposingPlayerTag.css("background-color",    ui.opposingPlayerColor);
    ui.opposingPlayerToggle.css("background-color", ui.opposingPlayerColor);

    //Set player tag names
    ui.playerTag.html(state.gamePlayerAlias);
    ui.opposingPlayerTag.html(opposingGamePlayerAlias);
  },
  
  /* Set the game message. This could be "Your Opponent has moved!"
  or "You scouted a 4" */
  appendMessage: function(msg) {
    //Pop the last element off the list if the max 
    //number of messages is reached
    var tMessages = $("#messageList li");
    if(tMessages.length >= ui.maxMessages) {
      tMessages.last().slideUp().remove();
    }
    
    var newMessage = $("<li>").html(msg);
    ui.messageList.prepend(newMessage);
    newMessage.slideDown();
  },
  
  /* Show the "Game Over" game board overlay. */
  showGameOverMessage : function() {
    var winningAlias = state.isPlayersTurn ? state.opposingGamePlayerAlias + 
                       " captured your flag!" : "You captured " + 
                       state.opposingGamePlayerAlias + "'s flag!";
    ui.gameOverMessage.html(winningAlias + "<br><br> Game Over <br> " + 
      (state.isPlayersTurn ? "<input id='rematchButton' type='button' value='Click here for a rematch!' />" : ""));
    ui.gameOverMessage.show();

    $("#rematchButton").click(function() {
      client.rematch(state.gameId, function(){util.jsForward('/game-created/');});
    });
    
  },
  
  /* Show the "Board is set... Waiting for opponent" game board overlay. */
  showBoardSetWaiting : function() {
    ui.setBoardMessage.hide();
    ui.boardSetWaitingMessage.show();   
  },
  
  /* Show the "Please set your board" game board overlay. */
  showSetBoard : function() {
    ui.waitingForPlayerMessage.hide();
    ui.setBoardMessage.show();
  },
  
  /* Show the "Ready to Play" game board overlay. */
  showReadyToPlay : function() {
    ui.setBoardMessage.hide();
    ui.boardSetWaitingMessage.hide();
    ui.waitingForPlayerMessage.hide();
    if(state.isPlayersTurn) {
      ui.gameOnMessage.html(ui.gameOnMessage.html()+"<br/><br/>It's your move!");
    }
    ui.gameOnMessage.show();
    util.setTimeout(function() {
      ui.gameOnMessage.fadeOut();
    },1500);
  },
  
  /* Set the last puece clicked. */
  setPieceClicked : function(slot) {
    state.lastSlotClicked = slot;
    //Make the text of the slot black to indicate it has been clicked
    slot.css("color", "#000");
  },
  
  /* Handle a slot click and either set the "lastSlotClicked",
  move the piece in board setup mode or make a move when the 
  game is on. Also, stop everything if the game is over. */
  handleSlotClick: function(slot) {
    //Need some better Game Over screen etc.
    if(state.gameStatus == "completed") {
      return;
    }
    
    //Clicking a slot with no previous slot selected
    if(state.lastSlotClicked == "") {
      ui.setPieceClicked(slot);

    //Piece selected... Player is tryign to make a move and
    //the board has been set
    } else if(state.boardSet) {
      if(state.isPlayersTurn) {
        var sx = state.lastSlotClicked.attr("x");
        var sy = state.lastSlotClicked.attr("y");
        var sr = state.lastSlotClicked.attr("r");
        var tx = slot.attr("x");
        var ty = slot.attr("y");
        var tr = slot.attr("r");
        
        //Only deal with moves involving the players pieces
        if(sr == "") {
          ui.resetLastSlotClicked(true); 
          return;
        }

        //Player is clicking their own piece to reset
        if(sx == tx && sy == ty) {
          ui.resetLastSlotClicked(true);
        } else {

          //Move is valid enough to send to server for final validaton
          //and state changes if applicable
          if(rules.isValidMove(sr,sx,sy,tr,tx,ty)) {           
            strategolobby.makeMove(state.gameId, state.gamePlayerId, 
                                   sx, sy, tx, ty);
          //Move is not valid
          } else {
            ui.resetLastSlotClicked(true);
          }
        }
      } else {
        ui.gameBoard.effect("shake", {times:2}, 75);
        ui.appendMessage("It's not your turn!");
        ui.resetLastSlotClicked(true);
      }
     
    //Still in board setup mode
    } else {
      if((state.isPlayer1    && 
          slot.attr("y") > 5 && 
          state.lastSlotClicked.attr("y") > 5) || 
         (!state.isPlayer1   && 
          slot.attr("y") < 4 && 
          state.lastSlotClicked.attr("y") < 4)) {
        ui.swapSlots(state.lastSlotClicked, slot);
        ui.resetLastSlotClicked(false);
      } else {
        ui.resetLastSlotClicked(true);
      }
    }
  },
  
  /* Rest the current "lastSlot" so the user can 
  select another piece. */
  resetLastSlotClicked : function(setColor) {
    if(setColor) {
      state.lastSlotClicked.css("color", "#FFF");
    }
    state.lastSlotClicked = "";
  },
  
  /* Swap two slots.. X, Y, and Rank. Change
  the color of the text for the rank in case
  we are moving into a empty slot. */
  swapSlots: function(slotA, slotB) {
    var tHtml  = slotA.html();
    var tRank  = slotA.attr("r");
    var tClass = slotA.attr("class");
    
    slotA.attr("class", slotB.attr("class"));
    slotA.css("color",  "#FFF");
    slotA.attr("r",     slotB.attr("r"));
    slotA.html(slotB.html());    

    slotB.attr("class", tClass);
    slotB.css("color",  "#FFF");
    slotB.attr("r",     tRank);
    slotB.html(tHtml);        
  },

  /* Build a temp piece to be used in animations etc. It should
  mirror the info passed to it. */
  getTempPiece : function(tClass, tTop, tLeft, tRank) {
    var tSlot = $("<div>");
    tSlot.addClass("tempPiece");
    tSlot.addClass(tClass);
    tSlot.css("top",    tTop+"px");
    tSlot.css("left",   tLeft+"px");
    tSlot.css("border", "0px");
    tSlot.html(tRank);
    return tSlot;
  },

  /* Return one of 8 top/left exit coordinate combos. */
  getRandomExitCoords: function() {    
    var eastX     =  500;
    var westX     = -500;
    var northY    =  500;
    var southY    = -500;

    var bTopDest  = 0;
    var bLeftDest = 0;

    switch(util.getRandomNumber(7)) {
      //Due East
      case 0: bTopDest = 0;      bLeftDest = eastX; break;
      //Due West
      case 1: bTopDest = 0;      bLeftDest = westX; break;
      //Due North
      case 2: bTopDest = northY; bLeftDest = 0;     break;
      //Due South
      case 3: bTopDest = southY; bLeftDest = 0;     break;
      //North East
      case 4: bTopDest = northY; bLeftDest = eastX; break;
      //North West
      case 5: bTopDest = northY; bLeftDest = westX; break;
      //South East
      case 6: bTopDest = southY; bLeftDest = eastX; break;
      //South West
      case 7: bTopDest = southY; bLeftDest = westX; break;
    }
    return {"top":bTopDest, "left":bLeftDest};
  },  

  /* Advance slotA to the posion of slotB and then remove slotA
  from the board via some exit coordinates. */
  advanceAndRemove: function(slotA, slotB, targetRank) {
    var aClass  = slotA.attr("class");   
    var aRank   = slotA.attr("r");
    var aCoords = slotA.offset();
    var aTop    = aCoords.top  + 2;
    var aLeft   = aCoords.left + 2;
    var bClass  = slotB.attr("class");
    var bRank   = slotB.attr("r");
    var bCoords = slotB.offset();
    var bTop    = bCoords.top  + 2;
    var bLeft   = bCoords.left + 2;

    slotA.attr("class", "slot");
    slotA.attr("r",     "");
    slotA.html("");
    
    //Reveal the targetRank if applicable
    if(targetRank != "" && bRank == "") {
      slotB.html(targetRank);
      util.setTimeout(function() {
        slotB.html("");
      }, 2000);
    }
    
    //Get a temp piece to animate
    var tSlotA = ui.getTempPiece(aClass, aTop, aLeft, aRank);
    ui.gameBoard.append(tSlotA);

    //Coords to move slotA to
    var aTopDest  = -1 * (aTop  - bTop);
    var aLeftDest = -1 * (aLeft - bLeft);

    //Coords to move slotA to FINAL
    var exitCoords     = ui.getRandomExitCoords();
    var finalLeftDest  = exitCoords.left;
    var finalTopDest   = exitCoords.top;
    
    //Animate the piece
    tSlotA.animate({
      left: '+='+aLeftDest,
      top:  '+='+aTopDest,
    }, 500, function() {        
      //Remove slotA from the board  
      tSlotA.animate({
        left: '+='+finalLeftDest,
        top:  '+='+finalTopDest,
      }, 500, function() {
        slotA.attr("class", "slot");
        slotA.attr("r",     "");
        slotA.html("");    
        tSlotA.remove();                  
      });   
    });
  },

  /* Advance slotA to slotB's position and replace slotB with slotA.
  slotB should exit the board via exit coordinates. */
  advanceAndReplace: function(slotA, slotB, targetRank) {
    var aClass  = slotA.attr("class");   
    var aRank   = slotA.attr("r");
    var aCoords = slotA.offset();
    var aTop    = aCoords.top  + 2;
    var aLeft   = aCoords.left + 2;

    var bClass  = slotB.attr("class");
    var bRank   = slotB.attr("r");
    var bCoords = slotB.offset();
    var bTop    = bCoords.top  + 2;
    var bLeft   = bCoords.left + 2;

    slotA.attr("class", "slot");
    slotA.attr("r",     "");
    slotA.html("");

    slotB.attr("class", "slot");
    slotB.attr("r",     "");
    slotB.html("");
        
    //Add temp pieces to the board for animation
    var tSlotA = ui.getTempPiece(aClass, aTop, aLeft, aRank);
    var tSlotB = ui.getTempPiece(bClass, bTop, bLeft, bRank);
    
    ui.gameBoard.append(tSlotA);
    ui.gameBoard.append(tSlotB);
    
    if(targetRank != "" && bRank == "") {
       tSlotB.html(targetRank);
    }
    
    //Coords to move slotA to
    var aTopDest   = -1 * (aTop  - bTop);
    var aLeftDest  = -1 * (aLeft - bLeft);

    //Coords to move slotB to
    var exitCoords = ui.getRandomExitCoords();
    var bLeftDest  = exitCoords.left;
    var bTopDest   = exitCoords.top;

    tSlotA.animate({
      left: '+='+aLeftDest,
      top:  '+='+aTopDest,
    }, 500, function() {      
      slotB.attr("class", aClass);
      slotB.attr("r",     aRank);
      slotB.html(aRank);
      slotB.css("color","#FFF");
      tSlotA.remove();
    });

    tSlotB.animate({
      left: '+='+bLeftDest,
      top:  '+='+bTopDest,
    }, 500, function() {
      slotA.attr("class", "slot");
      slotA.attr("r",     "");
      slotA.html("");    
      tSlotB.remove();
    });
  },

  /* Display an animation of the remaining pieces on the board flying
  off into random directions when a player finds a flag. */
  showGameOver: function() {
    ui.appendMessage("Game Over");

    var redBlue     = $(".red,.blue");
    var numElements = redBlue.length;
    var numAnimated = 0;

    redBlue.each(function() {
      var obj        = $(this);
      var exitCoords = ui.getRandomExitCoords();
      var leftDest   = exitCoords.left;
      var topDest    = exitCoords.top;
      var tCoords    = obj.offset();
      var tClass     = obj.attr("class");
      var tRank      = obj.attr("r");
      var tTop       = tCoords.top  + 2;
      var tLeft      = tCoords.left + 2;

      var tObj = ui.getTempPiece(tClass, tTop, tLeft, "");
      ui.gameBoard.append(tObj);

      obj.attr("class","slot");
      obj.attr("r", "");
      obj.html("");

      tObj.animate({
        left: '+='+leftDest,
        top:  '+='+topDest,
      }, 2000, function() {   
        tObj.remove();
        numAnimated++;

        //When the animation is complete refresh the page
        if(numAnimated == numElements) {
          top.location = top.location;
        }      
      });
    });    
  },
 
  /* Render the game board */
  renderBoard: function() {
    var rankCounts = {'F' : 1, '10': 1, '9' : 1, '8' : 2,
                      '7' : 3, '6' : 4, '5' : 4, '4' : 4,
                      '3' : 5, '2' : 8, 'S' : 1, 'B' : 6};       
    var pieces     = [];

    //Make sure the X or Y value is correct based on the player number
    var getPlayerSpecificCoord = function(xy, fromServer) {
      if(state.isPlayer1 || fromServer) {
        return xy;
      } else {
        return 9-xy;
      }
    };

    //Return the rank based on the passed X and Y coords
    var getBoardPositionRank = function(boardPositions, x, y) {
      for(var i=0; i < boardPositions.length; i++) {
        var pos = boardPositions[i];
        if(pos.x == x && pos.y == y) {
          return (pos.r == "" ? "X" : pos.r);
        }
      }
      return "";     
    };

    //Create and add a new board slot to the game board
    var createBoardSlot = function(isPlayer1, x, y, rank) {
      var colorClass = "";
    
      //Red Opponent Piece
      if(rank == "X" && isPlayer1) {
        colorClass = "red";
        rank       = "";
      //Blue Opponent Piece
      } else if(rank == "X" && !isPlayer1) {
        colorClass = "blue";
        rank       = "";
      //Blue Player Piece
      } else if(rank != "" && isPlayer1) {
        colorClass = "blue";
      //Red Player Piece
      } else if(rank != "" && !isPlayer1) {
        colorClass = "red";
      //Lake
      } else if((x == 2 || x == 3 || x == 6 || x == 7) && (y == 4 || y == 5)) {
        colorClass = "lightBlue";
      //Empty Slot
      } else {
        colorClass = "";
      }

      //Add element to board
      var slot = $("<div>");
      slot.addClass("slot");
      slot.addClass(colorClass);
      slot.attr("x", x);
      slot.attr("y", y);
      slot.attr("r", rank);
      slot.html(rank);

      ui.gameBoard.append(slot);
    };
    
    //Build a list of ranks
    for(var r in rankCounts) {
      var rc = rankCounts[r];
      for(var x=0; x < rc; x++) {
        pieces.push(r);
      }
    }
      
    //Populate the board
    for(var y=0; y < 10; y++) {
      for(var x=0; x < 10; x++) {       
        var ax   = getPlayerSpecificCoord(x);
        var ay   = getPlayerSpecificCoord(y);
        var rank = getBoardPositionRank(state.boardPositions,ax,ay);
       
        //Board setup
        if(rank             == ""        && 
           state.gameStatus != "started" && 
           state.gameStatus != "completed") {
          if(y < 4) {
            rank = "X";
          } else if (y > 5) {
            rank = pieces.pop();
          }
        }

        createBoardSlot(state.isPlayer1,ax,ay,rank);
      }
    }
  },  
  
  /* Return the board DOM element keyed by the passed 
  X and Y coordinate. */
  getSlotByCoord: function(x, y) {
    var slot = $(".slot[x="+x+"][y="+y+"]");
    if(slot) {
      return slot;
    } else {
      return "";
    }
  },

  /* Serialize the current board into JSON. */
  serializeBoardPositions: function() {
    var numPieces = 0;
    var json      = "[";
    $(".slot").each(function() {
      var slot = $(this);
      var r    = slot.html();
      if(r != "") {
        var x = slot.attr("x");
        var y = slot.attr("y");
        json += '{"x":'+x+',"y":'+y+',"r":"'+r+'"}';
        numPieces++;
        if(numPieces < 40) {
          json += ",";
        }        
      }   
    });
    json += "]";
    return json;
  }
}

var rules = {
  isValidMove: function(sr,sx,sy,tr,tx,ty) {
    var alertReturnFalse = function(msg) {
      ui.appendMessage(msg);
      ui.gameBoard.effect("shake", {times:2}, 75);
      return false;
    }

    var attackingOwn       = rules.isAttackingOwnPlayer(sr, tr);
    var movingImovable     = rules.isImovablePiece(sr);
    var movingIntoWater    = rules.isPieceInWater(tx,ty);
    var makingDiagonalMove = rules.isDiagonalMove(sx,sy,tx,ty);
    var makingMultiMove    = rules.isMultiSlotMove(sx,sy,tx,ty);
    var hoppingLake        = rules.isHoppingLake(sx,sy,tx,ty);
    var hoppingPlayer      = rules.isHoppingPlayer(sx,sy,tx,ty);

    if(attackingOwn) {
      return alertReturnFalse("You can't attack your own pieces");
    } else if(movingImovable) {
      return alertReturnFalse("You can't move an F or a B piece");
    } else if(movingIntoWater) {
      return alertReturnFalse("You can't move into a water slot");
    } else if(makingDiagonalMove) {
      return alertReturnFalse("You can't move a piece diagonally");
    } else if(sr != "2" && makingMultiMove) {
      return alertReturnFalse("Only 2's can move more than a single slot at a time");
    } else if(hoppingLake) {
      return alertReturnFalse("You can't hop the water slots");
    } else if(hoppingPlayer) {
      return alertReturnFalse("You can't hop another player");
    } else {
      return true;
    }
  },
  
  isAttackingOwnPlayer: function(sr,tr) {
    return sr != "" && tr != "";
  },
  
  isImovablePiece: function(sr) {
    return sr == "B" || sr == "F";
  },
  
  isPieceInWater: function(tx,ty) {
    return(tx == 2 ||
           tx == 3 ||
           tx == 6 ||
           tx == 7) &&
          (ty == 4 ||
           ty == 5);
  },
  
  isDiagonalMove: function(sx,sy,tx,ty) {
    return Math.abs(sx-tx) * Math.abs(sy-ty) != 0;
  },
  
  isMultiSlotMove: function(sx,sy,tx,ty) {
    var ax = Math.abs(sx-tx);
    var ay = Math.abs(sy-ty);
    return (ax != 1 && ax != 0)  || (ay != 1 && ay != 0);
  },
  
  isHoppingLake: function(sx,sy,tx,ty) {
    return ((sy  > 5 && ty < 4) && (sx == 2 || sx == 3)) || 
           ((sy  < 4 && ty > 5) && (sx == 2 || sx == 3)) ||
           ((sy  > 5 && ty < 4) && (sx == 6 || sx == 7)) ||
           ((sy  < 4 && ty > 5) && (sx == 6 || sx == 7)) ||
           ((sx  < 2 && tx > 3) && (sy == 4 || sy == 5)) ||
           ((sx  > 3 && tx < 2) && (sy == 4 || sy == 5)) ||
           ((sx  < 6 && tx > 7) && (sy == 4 || sy == 5)) ||
           ((sx  > 7 && tx < 6) && (sy == 4 || sy == 5));
  },
  
  isHoppingPlayer: function(sx,sy,tx,ty) {
    return false;    
  }
}

/******************************************************************************/
/**************** Manages the top level function of the game, *****************/
/******************************************************************************/
var strategolobby = {

  pollingStarted : false,
    
  /* Called to create a new game. A random game name will also be assigned here. */
  createNewGame: function(playerEmail, startAsPlayer1, opponentEmail,
                          captchaValue, captchaFailFunc) {
                              
    var success = function(obj) { 
      top.location = '/game-created/';
    };
    
    var error = function(json) {
      if(json == "captcha failed") {
        captchaFailFunc();
      }
    };
    
    client.createGame(startAsPlayer1, playerEmail, opponentEmail,
                      captchaValue, success, error);
  },
  
  /* Is this THE board page? */
  isBoardPage: function() {
    return window.location.pathname.indexOf("/board/") == 0;
  },
  
  /* Initialize the top level game logic. Wait 1sec before starting to poll
  for messages to make sure Google Chrome works correctly. */
  initialize: function(callBack) {            
    //If the board has a "Set Board" button on it, make sure it works
    var setBoardButton = $("#setBoardButton");
    if(setBoardButton.length > 0) {
      setBoardButton.click(function() {
        strategolobby.setBoard(state.gameId, state.gamePlayerId, 
                               ui.serializeBoardPositions());
      });
    }
      
    //Start long-polling for messages. Wait a seconds before starting for
    //Google Chrome which takes a bit to shake off the cob webs
    if(!strategolobby.pollingStarted) {
      util.setTimeout(function() {
        strategolobby.pollForMessages(state.gamePlayerId);
      }, 1000);
      
      strategolobby.pollingStarted = true;
    }
  },
  
  /* Poll for any new messages on the server. */
  pollForMessages: function(gamePlayerId) {
    client.checkMessages(
      gamePlayerId, 
      function(obj) {
        if(obj != null) {
          for(var r=0; r < obj.length; r++) {
            var response = obj[r];
            
            //The opposing player is making a move
            if(response.type == "makeMove" || 
               response.type == "opponentMakeMove") {
            
              strategolobby.movePiece(response);
            
            //The opposing player has set their board
            } else if(response.type    == "setBoard" && 
                      response.message == "readyForPlay") {
              state.boardSet           = true;
              state.gamePlayerBoardSet = true;
              state.gameStatus         = "started";
              ui.showReadyToPlay();            
            } else if(response.status == "error" && response.message == "outOfSync") {
              ui.appendMessage("Game board out of sync. Syncing with server.");
              ui.resetLastSlotClicked(true);
              util.refreshPage();
            }
          }
          
          //Wait 100ms before polling the sever again
          util.setTimeout(function() {
            strategolobby.pollForMessages(gamePlayerId);
          },100);
        } else {
          //In case of an error wait 15seconds and then refresh the entire page
          //to make sure all messaging is back in sync
          strategolobby.pollingStarted = false;
          util.setTimeout(function() {
            util.refreshPage();
          }, 100);
        }
      },
      
      function(error) {
        //In case of an error wait 15seconds and then refresh the entire page
        //to make sure all messaging is back in sync
        strategolobby.pollingStarted = false;        
        util.setTimeout(function() {
          util.refreshPage();
        }, 100);
      }
    );
  }, 
  
  /* The current player is committing their initial board setup. */
  setBoard: function(gameId, gamePlayerId, boardJson) {
    client.setGameBoard(gameId, gamePlayerId, boardJson, function(obj) {
      var message = obj.message;
      if(message == "readyForPlay") {
        state.boardSet   = true;
        state.gamePlayerBoardSet = true;
        state.gameStatus = "started";
        ui.showReadyToPlay();
      } else if(message == "boardSet" || message == "boardAlreadySet") {
        state.boardSet   = true;
        state.gamePlayerBoardSet = true;
        state.gameStatus = "started";
        ui.showBoardSetWaiting();
      }
    });
  },
  
  /* The current player is making a move. */
  makeMove: function(gameId, gamePlayerId, sx, sy, tx, ty) {
    client.makeMove(gameId, gamePlayerId, sx, sy, tx, ty, function(obj) {
      strategolobby.movePiece(obj);
    });
  },

  /* Log for moving a piece on the board. This function should be called
  whenever a piece is moved outside of board setup mode. */ 
  movePiece : function(obj) {
    var status      = obj.status;
    var message     = obj.message;

    if(status == "success") {
      var sx          = obj.sx;
      var sy          = obj.sy;
      var tx          = obj.tx;
      var ty          = obj.ty;
      var tr          = obj.tr;
      var moveMessage = obj.moveMessage;
      var type        = obj.type;
      var lastMove    = obj.lastMove;

      state.lastMove      = lastMove;
      state.lastMoveFromX = sx;
      state.lastMoveFromY = sy;
      state.lastMoveToX   = tx;
      state.lastMoveToY   = ty;

      var lastSlot = ui.getSlotByCoord(sx, sy);
      var slot     = ui.getSlotByCoord(tx, ty);

      if(message == "advance_piece") {
        if(tr == "F") {
          ui.showGameOver();
        } else {
          ui.advanceAndReplace(lastSlot, slot, tr);
        }             
      } else if(message == "remove_piece") {
        if(type == "makeMove") {
          var sr = lastSlot.attr("r");
          ui.appendMessage(sr + " Lost");
        }
        ui.advanceAndRemove(lastSlot, slot, tr);
      } else if(message == "no_piece_interaction") {
        ui.advanceAndReplace(lastSlot, slot);
      }

      if(moveMessage && !(type == "makeMove" && moveMessage == "Game Over")) {
        ui.appendMessage(moveMessage);
      }

      ui.resetLastSlotClicked(false);
      ui.toggleMoveFlag();

    } else if(status == "error" && message == "outOfSync") {
      ui.appendMessage("Game board out of sync. Syncing with server.");
      ui.resetLastSlotClicked(true);
      util.refreshPage();
    } else {
      //Not a valid move
      ui.resetLastSlotClicked(true);
    }
  }
}

/******************************************************************************/
/**************** All the crud to run once the page is loaded *****************/
/******************************************************************************/
$(document).ready(function() {
    
  //Initialize modal "How to Play"
  $('#howToPlay,#missingEmail,#selectSide,#missingOpposingEmail,#captchaWrong').jqm({toTop:true});
    
  //Setup the "Create game" button
  if(util.exists($("#createGameButton"))) {
    var form = document.createForm;
    
    //Set the user's focus to the game name when the page is loaded
    form.playerEmail.focus();
    
    //Acitvate regenerate captcha button
    $("#regenCaptcha").click(function() {    
      var queryString = "?playerEmail="   + form.playerEmail.value   +
                        "&playerColor="   + form.playerColor.value   +
                        "&opponentEmail=" + form.opponentEmail.value ;
      top.location = window.location.pathname + queryString;
    });
    

    //Attempt to pre-populate the skill and alias fields
    form.playerEmail.value   = util.getParameterByName("playerEmail");
    form.playerColor.value   = util.getParameterByName("playerColor");
    form.opponentEmail.value = util.getParameterByName("opponentEmail");
    
    //Bind the click function to the create game button
    $("#createGameButton").click(function() {
      var playerEmail   = form.playerEmail.value;
      var playerColor   = form.playerColor.value;
      var opponentEmail = form.opponentEmail.value;
      var captchaValue  = form.captchaValue.value;
      var player1       = playerColor == "blue";

      var captchaFailFunc = function() {
        $('#captchaWrong').jqmShow();
      }
      
      if(playerEmail == "") {
        $('#missingEmail').jqmShow();
      } else if(playerColor == "") {
        $('#selectSide').jqmShow();
      } else if(opponentEmail == "") {
        $('#missingOpposingEmail').jqmShow();        
      } else if(captchaValue == "") {
        $('#captchaWrong').jqmShow();
      } else {    
        strategolobby.createNewGame(playerEmail, player1, opponentEmail, 
                                    captchaValue,captchaFailFunc);
      }
    });
  }
  
  $("#globalLeaderboardButton").click(function() {
    util.jsForward("/leaderboard");
  });
    
  $("#globalStartGameButton").click(function() {
    util.jsForward('/');
  });
    
  $("#globalHowToPlayButton").click(function() {
    $('#howToPlay').jqmShow();   
  });
  
  $("#closeHowToPlay").click(function() {
    $('#howToPlay').jqmHide();
  });

  $("#closeMissingEmail").click(function() {
    $('#missingEmail').jqmHide();
        form.playerEmail.focus();
  });

  $("#closeSelectSide").click(function() {
    $('#selectSide').jqmHide();
        form.playerColor.focus();
  });

  $("#closeMissingOpposingEmail").click(function() {
    $('#missingOpposingEmail').jqmHide();
        form.opponentEmail.focus();
  });

  $("#closeCaptchaWrong").click(function() {
    $('#captchaWrong').jqmHide();
    form.captchaValue.focus();
  });

  //Add click handler to "Last Move" button on board page
  $("#boardHighlightLastMoveBytton").click(function() {
    ui.hightlightLastMove()
  });

  util.refreshPage();
});
