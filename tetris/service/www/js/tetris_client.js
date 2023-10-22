/*
        @licstart  The following is the entire license notice for the
        JavaScript code in this page.

        Copyright (C) 2014  Ernesto Alfonso

        The JavaScript code in this page is free software: you can
        redistribute it and/or modify it under the terms of the GNU
        General Public License (GNU GPL) as published by the Free Software
        Foundation, either version 3 of the License, or (at your option)
        any later version.  The code is distributed WITHOUT ANY WARRANTY;
        without even the implied warranty of MERCHANTABILITY or FITNESS
        FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

        As additional permission under GNU GPL version 3 section 7, you
        may distribute non-source (e.g., minimized or compacted) forms of
        that code without the copy of the GNU GPL normally required by
        section 4, provided you include this license notice and a URL
        through which recipients can access the Corresponding Source.


        @licend  The above is the entire license notice
        for the JavaScript code in this page.
*/

var UI = function(parentElt, config) {

    this.config = config || UI.DEFAULT_CONFIG;
    this.cellGrid = [];
    this.loading = createElementWithProperties("img", {
        hw: ["400", "550"],
        show: function(show) {
            if (show) {
                this.height = this.hw[0];
                this.width = this.hw[1];
                this.style = "";
            } else {
                this.height = "0";
                this.width = "0";
                this.style = "visibility:hidden";

            }
        }
    });
    this.moveNoElm = null;
    this.parentElt = parentElt;
    this.tableParentDiv = document.createElement("div");

    this.init();
};

UI.prototype.paint = function(r, c, color) {
    assert(color != null);
    this.cellGrid[r][c].bgColor = color !== OFF? UI.COLORS.FILLED: UI.COLORS.BLANK;
};

UI.DEFAULT_CONFIG = {
    borderStyle: "1px solid #000",
    cellSize: "30",
    fontSize: "30px",
    borderWidth: "20"
}

UI.prototype.tableCreate = function(parentElt, width, height) {
    var body = parentElt;

    this.loading.show(false);
    body.appendChild(this.loading);

    var tbl = document.createElement("table");
    tbl.class = "table";
    var tblBody = document.createElement("tbody");

    var cellRow;
    for (var j = 0; j < height; j++) {
        cellRow = [];
        this.cellGrid.push(cellRow);
        var row = document.createElement("tr");
        for (var i = 0; i < width; i++) {
            var cell = document.createElement("td");
            cellRow.push(cell);

            cell.width = this.config.cellSize;
            cell.height = this.config.cellSize;
            cell.bgColor = UI.COLORS.BLANK;
            cell.style.border = this.config.borderStyle;

            var cellText = document.createTextNode("");
            cell.appendChild(cellText);
            row.appendChild(cell);
        }

        tblBody.appendChild(row);
    }
    tbl.appendChild(tblBody);
    body.appendChild(tbl);
    tbl.setAttribute("border", this.config.borderWidth);
};

UI.prototype.init = function() {

    // add table div, move counter and slider
    var body = this.parentElt;
    body.appendChild(this.tableParentDiv);

    body.appendChild(createElementWithProperties(
        "label", {
            innerHTML: "Move ",
            "style.fontSize": this.config.fontSize
        }));
    this.moveNoElm = (createElementWithProperties(
        "label", {
            "style.fontSize": this.config.fontSize
        }));
    body.appendChild(this.moveNoElm);

    body.appendChild(document.createElement("br"));

    body.appendChild(createElementWithProperties(
        "label", {
            innerHTML: "Speed ",
            "style.fontSize": this.config.fontSize
        }));

    this.slider = createElementWithProperties(
        "input", {
            type: "range",
        });


    body.appendChild(this.slider);
};

UI.prototype.initSlider = function(initialValue, onChangeFun) {

    this.slider.min = 1;
    this.slider.max = 100;

    this.slider.invertValue = function(val) {
        return parseInt(this.max) - val + parseInt(this.min);
    };
    this.slider.onchange = function() {
        onChangeFun(this.invertValue(this.value));
    };

    this.slider.value = this.slider.invertValue(initialValue);
};

UI.COLORS = [
    "#ffffff", // WHITE
    "#0000f0", // BLUE
    '000000' // BLACK
];

UI.COLORS.BLANK = UI.COLORS[0];
UI.COLORS.FILLED = UI.COLORS[1];

UI.prototype.paintTo = function(b, color) {
    // paint cells occupied by block b with the given color
    for (var itr = b.iter(); itr.hasNext();) {
        var xy = itr.next().value;
        this.paint(xy[1], xy[0], color);
    }
    return true;
};

UI.prototype.repaintRows = function(ymin, ymax, grid) {
    // repaint UI rows from ymin to ymax, with grid as reference
    for (; ymin < ymax; ymin++) {
        for (var x = 0; x < grid.width; x++) {
            // TODO
            this.paint(ymin, x, grid.g[ymin][x]);
        }
    }
};

var INITIAL_TIMER_DELAY = 90;

var Game = function(parentElt, uiConfig) {
    this.b = new Block(); // the currently active block
    this.answer = new Block(); // the best-move answer from the AI server
    this.ui = new UI(parentElt, uiConfig);

    this.pausedP = false;
    this.gameOver = false;
    this.moveNo = null;
    this.gameNo = null;
    this.shapes = null; // the shape configurations from the server

    this.grid = null;
    this.timerDelay = INITIAL_TIMER_DELAY; // ms delay between moves
};

const OFF = 0;
const ON = 1;

var Grid = function(height, width) {
    this.height = height;
    this.width = width;

    this.rowcounts = [];
    this.g = [];
    this.relief = []; // track the tallest ON cell for each column

    this.needClear = []; //list of rows that need to be cleared

    // initialize the HxW grid
    var i;
    for (i = 0; i < this.height; i++) {
        this.rowcounts.push(0);
        var row = [];
        this.g.push(row);
        for (var ii = 0; ii < this.width; ii++)
            row.push(OFF);
    }

    for (i = 0; i < this.width; i++) {
        this.relief.push(this.height);
    }
};

Grid.prototype.setCell = function(y, x, val){
    // set grid cell to a given value, keeping invariants
    this.g[y][x] = val;

    if (val !== OFF)    {
        if (y < this.relief[x]) {
            this.relief[x] = y;
        }
        this.rowcounts[y]++;
    }else     {
        this.rowcounts[y]--;
        if (y == this.relief[x])    {
            var i;
            for (i = y-1; i>=0 && this.g[y][x] == Grid.OFF; i--);
            this.relief[x] = i;
        }
    }
}

Grid.prototype.getDropDistance = function(b) {
    // return the max number of units b can move down
    var botCrust = b.shape.rotations[b.r].crusts.bot;

    var dist, minDist = this.height;

    for (var i = 0, reliefY; i < botCrust.length; i++) {
        var xy = botCrust[i];
        reliefY = this.relief[xy[0] + b.x];
        dist = reliefY - xy[1];
        if (dist < minDist) {
            minDist = dist;
        }
    }

    var dropDist = minDist - 1 - b.y;
    return dropDist;
};

Grid.prototype.drop = function(b) {
    // drop b onto the grid and add it
    var dropDistance = this.getDropDistance(b);
    if (dropDistance < 0) {
        return false;
    } else {

        b.y += dropDistance;

        var topCrust = b.shape.rotations[b.r].crusts.top;
        var xy;
        for (var i = 0; i < topCrust.length; i++) {
            xy = topCrust[i];
            this.relief[xy[0] + b.x] = xy[1] + b.y;
        }

        for (var itr = b.iter(); itr.hasNext();) {
            xy = itr.next().value;
            if (++this.rowcounts[xy[1]] == this.width) {
                this.needClear.push(xy[1]);
            }
            this.g[xy[1]][xy[0]] = ON;
        }
        return true;
    }
};

Grid.prototype.clearLines = function(ui) {

    // clear full lines in grid, as well as in `ui' if provided

    if (this.needClear.length == 0) return;

    var cmpNum = function(a, b) {
        return a - b;
    };
    // this.lastCleared = this.needClear.length;
    if (this.needClear.length > 0) {
        // cmpNum is necessary, otherwise sort is lexicographic, eg 10<9
        // would be nice to make needClear a pqueue
        this.needClear.sort(cmpNum); //smallest to largest
    }
    var YMIN = this.needClear[this.needClear.length - 1];
    var YMAX = listMin(this.relief); //the tallest row is 0. ymax should be as small as possible
    var y = YMIN;
    // this.needClear.reverse ();
    var nextNonFull = y - 1; //not necessarily non-full here
    var cleared = [];

    while (nextNonFull >= YMAX) {
        while (this.rowcounts[nextNonFull] == this.width)
            nextNonFull -= 1;
        if (nextNonFull < YMAX)
            break;
        //nextNonFull should be non-full now
        if (this.rowcounts[y] == this.width) {
            assert(this.needClear[this.needClear.length - 1] == y);
            this.needClear.pop();
            cleared.push(this.g[y]);
        }
        //copy nextNonFull row into y-th row
        this.g[y] = this.g[nextNonFull];
        this.rowcounts[y] = this.rowcounts[nextNonFull];
        y -= 1;
        nextNonFull -= 1;
    }

    while (this.needClear.length > 0)
        cleared.push(this.g[this.needClear.pop()]);

    while (y >= YMAX) {
        // assert((cleared.length>0  && sum(cleared[0])==this.width)
        // || sum(this.g[y])==this.width);

        assert(cleared.length > 0, "cleared.length > 0 failed");
        this.g[y] = cleared.pop();
        this.rowcounts[y] = 0;
        for (i = 0; i < this.width; i++)
            this.g[y][i] = OFF;
        y -= 1;
    }

    assert(this.needClear.length == 0);
    assert(cleared.length == 0);

    for (var i = 0; i < this.width; i++) {
        var relief = this.relief[i];
        while (relief < this.height && this.g[relief][i] == OFF)
            relief += 1;
        this.relief[i] = relief;
    }

    if (ui != null) {
        ui.repaintRows(YMAX, YMIN + 1, this);
    }
};

Grid.prototype.blockIntersects = function(b) {
    // determine whether the floating block `b' overlaps with any ON cell in the grid
    for (var itr = b.iter(); itr.hasNext();) {
        var xy = itr.next().value;
        if (this.g[xy[1]][xy[0]] != OFF) {
            return true;
        }
    }
    return false;
};

var Block = function(m, r, y, x, shape) {
    // a 'tetro', here called Block

    this.m = m; // the shape or 'model' index
    this.r = r; // the rotation
    this.y = y; // the row index
    this.x = x; // the column index
    this.shape = shape; // the shape configuration obect for model `this.m'
};

Block.prototype.iter = function() {
    // return an iterator over the block cells
    var b = this;
    return (function() {
        var i = 0;

        var x = b.x;
        var y = b.y;
        var rotCoords = b.shape.rotations[b.r].configurations;

        var cont = {
            value: [null, null],
            done: false
        };
        var done = {
            done: true
        };
        return {
            next: function() {
                if (i < rotCoords.length) {
                    cont.value[0] = rotCoords[i][0] + x;
                    cont.value[1] = rotCoords[i][1] + y;
                    i++;
                    return cont;
                } else {
                    return done;
                }
            },
            hasNext: function() {
                return i < rotCoords.length;
            }
        };
    })();
};

Game.prototype.logPerformance = function() {
    // periodically log moves/sec
    var MOD = 1000;
    if (this.moveNo % MOD == 0) {
        var now = window.performance.now();
        if (this.last != null) {
            var elapsed = (now - this.last) / 1000;
            console.log("moves/sec: " + precisionRound(MOD / elapsed, 2));
        }
        this.last = now;
    }
};

Game.prototype.fetchCallback = function(move) {
    // set the fetched next block/move to display
    assert(this.gameNo != null && this.moveNo != null);

    // reset the block to the new shape and center it at the top and middle
    this.b.m = move.m;
    this.b.shape = this.shapes[this.b.m];
    this.b.r = 0;
    this.b.x = this.grid.width / 2 - 1;
    this.b.y = 0;

    this.moveNo+=1;
    this.logPerformance();
    // update UI move number
    this.ui.moveNoElm.innerHTML = this.moveNo;
};

Game.prototype.fetch = function() {
    // send request to fetch the next block and the AI best move
    var game = this;
    if (this.ws != null) {
        return new Promise(function(resolve, reject) {
            game.ws.resolve = resolve;
            game.ws.reject = reject;
            game.ws.send(game.moveNo);
        });
    } else {
        var uri = "/games/" + this.gameNo + "/moves/" + this.moveNo;
        return serverRequest(uri).then(fetchCallback, gameOver);
    }
};

Game.prototype.init = function(gameNo) {
    // initialize the game
    this.gameNo = gameNo;

    // use 'state' as 'this' to distinguish from game

    var game = this;
    return serverRequest("/games/" + gameNo)
        .then(function(response) {
            var resp = response;

            game.moveNo = resp.move_no;
            // resp.moveNo is for current move, need to add 1 for next move
            game.moveNo++;

            var START_FROM_FIRST_MOVE = false;
            if (START_FROM_FIRST_MOVE) {
                resp.move_no = 0;
                resp.on_cells = [];
            }

            console.log("move no is: " + game.moveNo);

            game.initCells(resp.height, resp.width, resp.on_cells);

            game.ui.initSlider(this.timerDelay, (function(newVal) {
                game.timerDelay = newVal;
            }));

            var supportsWebSockets = 'WebSocket' in window || 'MozWebSocket' in window;

            if (supportsWebSockets && resp.ws_port) {
                var scheme = window.location.protocol == "https:"? "wss": "ws";
                var ws_url = `${scheme}://${window.location.hostname}:${resp.ws_port}/ws/games/${game.gameNo}`;
                return game.initWs(ws_url);
            }
        });
};

Game.prototype.initCells = function(height, width, onCells){
    // initialize the game.grid cells and the UI cells

    // onCells from server are packed, also vertically flipped wrt JS representation

    this.grid = new Grid(height, width);

    //TODO delete previous table
    this.ui.tableCreate(this.ui.tableParentDiv, width, height);

    // initialize grid and UI grid
    var xy, x, y;
    var miny = height;

    for (var i = 0; i < onCells.length; i++) {
        // unpack server cell
        xy = onCells[i];
        x = xy % width;
        y = Math.floor(xy / width);
        // flip y upside down from server representation
        y = height - 1 - y;

        this.grid.setCell(y, x, ON);
        this.ui.paint(y, x, ON);
        if (y < miny) miny = y;
    }
    // repaint remaining UI rows from grid
    this.ui.repaintRows(0, miny, this.grid);
}

Game.prototype.initWs = function(ws_url){
    // initialize the websocket connection
    var state = this;
    return new Promise(function(resolve, reject) {
        state.ws_url = ws_url;
        console.log("using ws url: " + state.ws_url);
        state.ws = new WebSocket(state.ws_url);
        state.ws.addEventListener('message', function(event) {
            var packed = event.data;
            // if (packed<0)    {state.ws.reject();}
            var answer = state.answer;
            answer.m = (packed >> 16) & 0xff;
            answer.r = (packed >> 8) & 0xff;
            answer.x = (packed >> 0) & 0xff;
            state.fetchCallback(answer);
            state.ws.resolve();
        });
        state.ws.addEventListener('open', function(event) {
            console.log("ws connection opened..");
            resolve();
        });

        state.ws.addEventListener('error', function(event) {
            reject("ws connection error..");
        });

        state.ws.addEventListener('close', function(event) {
            reject("ws connection closed..");
        });
    });
};

Game.prototype.fetchShapes = function() {
    // fetch the shape configurations used by the server
    var game = this;
    return serverRequest("shapes")
        .then(function(response) {
            var shapes = response;
            game.shapes = shapes;
            if (shapes.length == 0) {
                throw new Error("0 shapes received from server!");
            }
            // the purpose of the loop below is to vertically flip the shapes
            // since the server represenation is vertically flipped wrt client
            for (var i = 0; i < shapes.length; i++) {
                var shape = shapes[i];
                var rots = shape.rotations;
                // TODO encapsulate
                for (var r = 0; r < rots.length; r++) {
                    var zeroSeen = false;
                    var rot = rots[r];
                    var rotH = rot.height; // (rot.height -1) is the max Y value, mapped to 0
                    var rotCoords = rot.configurations;
                    var b, cr;
                    for (b = 0; b < rotCoords.length; b++) {
                        cr = rotCoords[b];
                        cr[1] *= -1;
                        cr[1] += rotH - 1;
                        assert(cr[1] >= 0);
                        zeroSeen = zeroSeen || cr[1] == 0;
                    }
                    assert(zeroSeen);

                    var CRUSTNAMES = ["top", "bot", "left", "right"];
                    for (var c = 0; c < CRUSTNAMES.length; c++) {
                        var crust = rot.crusts[CRUSTNAMES[c]];
                        for (b = 0; b < crust.length; b++) {
                            cr = crust[b];
                            cr[1] *= -1;
                            cr[1] += rotH - 1;
                            assert(cr[1] >= 0);
                        }
                    }
                }
            }
        });
};

Game.prototype.fetchGameNo = function() {
    // fetch a currently active game number from the server
    return serverRequest("/games").then(function(response) {
        var gamenoList = response;
        if (gamenoList.length == 0) {
            throw new Error("no current games on server");
        } else {
            var gameNo = gamenoList[gamenoList.length - 1];
            console.log("init gameNo is " + gameNo);
            return gameNo;
        }
    });
};

Game.prototype.planExecute = function() {
    // execute a plan to get the currently active block
    // into the AI-best-move state
    var game = this;
    return new Promise(function(resolve, reject) {
        game.planExecuteCallback(function() {
            resolve();
        }, reject);
    });
};

Game.prototype.planExecuteCallback = function(resolve, reject) {
    // make moves one by one until game.b looks like game.answer
    // with game.timerDelay in between moves
    var game = this;
    var b = game.b;
    var answer = game.answer;
    var ui = this.ui;

    this.ui.paintTo(b, OFF); // erase
    var origR = b.r;
    var origX = b.x;

    if (b.r != answer.r) {
        b.r++;
        b.r %= 4;
    } else if (b.x < answer.x) {
        b.x++;
    } else if (b.x > answer.x) {
        b.x--;
    } else {
        // we are done. drop and resolve the promise
        if (!this.grid.drop(this.b)) {
            // this should never happen since block is not intersecting the grid
            assert(false);
            game.gameOver = true;
            reject("cannot drop");
        } else {
            this.ui.paintTo(b, ON);
            this.grid.clearLines(this.ui);
            setTimeout(resolve, this.timerDelay);
        }
        return;
    }

    // check if move is possible
    if (this.grid.blockIntersects(b)) {
        // unable to move block. undo last move and repaint
        b.r = origR;
        b.x = origX;
        this.ui.paintTo(b, ON);
        game.gameOver = true;
        reject("cannot place block");
    } else {
        // continue with plan
        this.ui.paintTo(b, ON);
        setTimeout(this.planExecuteCallback.bind(this, resolve, reject), this.timerDelay);
    }
};

Game.prototype.pauseToggle = function() {
    this.pausedP = !this.pausedP;
};

Game.prototype.gameOver = function() {
    alert("game over!");
};

Game.prototype.fetchPlanExecuteLoop = function() {
    // a recursive promise to continuously fetch, plan, execute
    var game = this;
    this.fetch()
        .then(this.ui.paintTo.bind(this.ui, game.b, ON)) // add active block to the UI
        .then(this.planExecute.bind(this))
        .then(this.fetchPlanExecuteLoop.bind(this));
};

Game.prototype.start = function() {
    this.fetchGameNo()
        .then(this.init.bind(this))
        .then(this.fetchShapes.bind(this))
        .then(this.fetchPlanExecuteLoop.bind(this))
        .catch(handleError);
};

var games = [];
window.onload = function() {
    var parentElt = document.getElementsByTagName("body")[0];
    assert(parentElt);
    var game = new Game(parentElt);
    games.push(game);
    game.start();
};
