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

var UI = function(parentElt) {

    this.cellSize = "30";
    this.cellGrid = [];
    this.fontSize = "30px";
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

    this.init();
};

UI.prototype.paint = function(r, c, color) {
    assert(color != null);
    this.cellGrid[r][c].bgColor = color;
};

UI.prototype.tableCreate = function(parentElt, width, height) {
    var body = parentElt;

    this.loading.show(false);
    body.appendChild(this.loading);

    var tbl = document.createElement("table");
    tbl.class = "table";
    var tblBody = document.createElement("tbody");

    for (var j = 0; j < height; j++) {
        cellRow = [];
        this.cellGrid.push(cellRow);
        var row = document.createElement("tr");
        for (var i = 0; i < width; i++) {
            var cell = document.createElement("td");
            cellRow.push(cell);

            cell.width = this.cellSize;
            cell.height = this.cellSize;
            cell.bgColor = this.colors.blank;
            cell.style.border = "1px solid #000";

            var cellText = document.createTextNode("");
            cell.appendChild(cellText);
            row.appendChild(cell);
        }

        tblBody.appendChild(row);
    }
    tbl.appendChild(tblBody);
    body.appendChild(tbl);
    tbl.setAttribute("border", "2");
};

UI.prototype.init = function() {

    var body = this.parentElt;
    body.appendChild(createElementWithProperties(
        "label", {
            innerHTML: "Move ",
            "style.fontSize": this.fontSize
        }));
    this.moveNoElm = (createElementWithProperties(
        "label", {
            "style.fontSize": this.fontSize
        }));
    body.appendChild(this.moveNoElm);

    body.appendChild(document.createElement("br"));

    body.appendChild(createElementWithProperties(
        "label", {
            innerHTML: "Speed ",
            "style.fontSize": this.fontSize
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

UI.prototype.colors = {
    'BLUE': "#0000f0",
    'BLACK': "#000000",
    'WHITE': "#ffffff",
    'GREEN': 3,
    filled: this.BLUE,
    blank: this.WHITE
};

UI.prototype.colors.filled = UI.prototype.colors.BLUE;
UI.prototype.colors.blank = UI.prototype.colors.WHITE;

UI.prototype.paintTo = function(b, color) {
    for (var itr = b.iter(); itr.hasNext();) {
        var xy = itr.next().value;
        this.paint(xy[1], xy[0], color);
    }
    return true;
};

UI.prototype.repaintRows = function(ymin, ymax, grid) {
    for (; ymin < ymax; ymin++) {
        for (var x = 0; x < grid.width; x++) {
            // TODO
            this.paint(ymin, x, grid.g[ymin][x] || this.colors.blank);
        }
    }
};

var INITIAL_TIMER_DELAY = 90;

var Game = function(parentElt) {
    this.b = new Block();
    this.answer = new Block();
    this.ui = new UI(parentElt);

    this.pausedP = false;
    this.gameOver = false;
    this.moveNo = null;
    this.gameNo = null;
    this.shapes = null;
    this.consecFailedMills = 0;

    this.grid = null;
    this.move = new Object();
    this.timerDelay = INITIAL_TIMER_DELAY;
};

var Grid = function(height, width) {
    this.height = height;
    this.width = width;

    this.rowcounts = [];
    this.g = [];
    this.relief = [];

    this.needClear = [];
    this.needsClear = false;

    for (var i = 0; i < this.height; i++) {
        this.rowcounts.push(0);
        var row = [];
        this.g.push(row);
        for (var ii = 0; ii < this.height; ii++)
            row.push(UI.prototype.colors.blank);
    }
    for (var i = 0; i < this.width; i++) {
        this.relief.push(this.height);
    }
};

Grid.prototype.getDropDistance = function(b) {
    var botCrust = b.shape.rotations[b.r].crusts.bot;

    var dist, minDist = this.height;
    var x = b.x;
    var y = b.y;

    for (var i = 0, reliefY; i < botCrust.length; i++) {
        var xy = botCrust[i];
        var reliefY = this.relief[xy[0] + b.x];
        var dist = reliefY - xy[1];
        if (dist < minDist) {
            minDist = dist;
        }
    }

    var dropDist = minDist - 1 - b.y;
    return dropDist;
};

Grid.prototype.drop = function(b) {
    var dropDistance = this.getDropDistance(b);
    if (dropDistance < 0) {
        return false;
    } else {

        b.y += dropDistance;

        var topCrust = b.shape.rotations[b.r].crusts.top;
        for (var i = 0, xy = null; i < topCrust.length; i++) {
            xy = topCrust[i];
            this.relief[xy[0] + b.x] = xy[1] + b.y;
        }

        for (var itr = b.iter(); itr.hasNext();) {
            var xy = itr.next().value;
            if (++this.rowcounts[xy[1]] == this.width) {
                this.needsClear = true;
                this.needClear.push(xy[1]);
            }
            this.g[xy[1]][xy[0]] = UI.prototype.colors.filled;
        }
        return true;
    }
};

Grid.prototype.clearLines = function(ui) {

    if (!this.needsClear) return;

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
            assert(this.needClear[this.needClear.length - 1] == y, " assertion failed at 485 ");
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

        assert(cleared.length > 0, " cleared.length assertion ");
        this.g[y] = cleared.pop();
        this.rowcounts[y] = 0;
        for (var i = 0; i < this.width; i++)
            // TODO use 0, 1
            this.g[y][i] = UI.prototype.colors.blank;
        y -= 1;
    }

    assert(this.needClear.length == 0, " this.needClear.length==0 assertion failed");
    assert(cleared.length == 0, "cleared.length==0 assertion failed");

    for (var i = 0; i < this.width; i++) {
        var relief = this.relief[i];
        while (relief < this.height && this.g[relief][i] == UI.prototype.colors.blank)
            relief += 1;
        this.relief[i] = relief;
    }

    this.needsClear = false;
    if (ui != null) {
        ui.repaintRows(YMAX, YMIN + 1, this);
    }
};

Grid.prototype.blockIntersects = function(b, ui) {
    // TODO remove ui param
    for (var itr = b.iter(); itr.hasNext();) {
        var xy = itr.next().value;
        assert(ui.cellGrid[xy[1]][xy[0]].bgColor == UI.prototype.colors.blank ||
            this.grid.g[xy[1]][xy[0]] == ui.colors.filled);
        if (ui.cellGrid[xy[1]][xy[0]].bgColor != UI.prototype.colors.blank) {
            return true;
        }
    }
    return false;
};

var Block = function(m, r, y, x, shape) {
    this.m = m;
    this.r = r;
    this.y = y;
    this.x = x;
    this.shape = shape;
};

Block.prototype.iter = function() {
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
    assert(this.gameNo != null && this.moveNo != null);
    // todo wrap
    this.b.m = move.shape, this.b.r = 0, this.b.x = this.grid.width / 2 - 1, this.b.y = 0;
    this.b.shape = this.shapes[this.b.m];
    this.answer.r = move.rot, this.answer.x = move.col;
    this.moveNo++;
    this.logPerformance();
    this.ui.moveNoElm.innerHTML = this.moveNo;
};

Game.prototype.fetch = function() {
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
    this.gameNo = gameNo;

    // use 'state' as 'this' to distinguish from game. TODO

    var state = this;
    return serverRequest("/games/" + gameNo)
        .then(function(response) {
            var game = response;


            var miny = game.height;

            var skip = false; // start from move 0
            if (skip) {
                game.move_no = -1;
                game.on_cells = [];
            }

            state.moveNo = game.move_no;

            // game.moveNo is for current move.
            state.moveNo++;

            var grid = new Grid(game.height, game.width);
            state.grid = grid;

            console.log("move no is: " + state.moveNo);

            var supportsWebSockets = 'WebSocket' in window || 'MozWebSocket' in window;

            if (supportsWebSockets && game.ws_port) {
                state.ws_url = "ws://" + window.location.hostname + ":" + game.ws_port +
                    "/games/" + state.gameNo;
                console.log("using ws url: " + state.ws_url);
                state.ws = new WebSocket(state.ws_url);
                state.ws.addEventListener('message', function(event) {
                    var packed = event.data;
                    // if (packed<0)    {state.ws.reject();}
                    var move = state.move;
                    move.shape = (packed >> 16) & 0xff;
                    move.rot = (packed >> 8) & 0xff;
                    move.col = (packed >> 0) & 0xff;
                    state.fetchCallback(move);
                    state.ws.resolve();
                });
            }

            //delete previous table
            state.ui.tableCreate(state.ui.parentElt, grid.width, grid.height);

            for (var i = 0; i < game.on_cells.length; i++) {
                xy = game.on_cells[i];
                x = xy % grid.width;
                y = Math.floor(xy / grid.width);
                y = grid.height - 1 - y;

                grid.g[y][x] = UI.prototype.colors.filled;
                if (y < miny)
                    miny = y;
                if (y < grid.relief[x]) {
                    grid.relief[x] = y;
                }
                grid.rowcounts[y]++;
            }
            state.ui.repaintRows(0, grid.height, grid);
            state.ui.initSlider(this.timerDelay,
                (function(newVal) {
                    state.timerDelay = newVal;
                }).bind(this));
            if (state.ws) {
                return new Promise(function(resolve, reject) {
                    state.ws.addEventListener('open', function(event) {
                        console.log("ws connection opened..");
                        resolve();
                    });

                    state.ws.addEventListener('error', function(event) {
                        console.log("ws connection error..");
                        reject();
                    });

                    state.ws.addEventListener('close', function(event) {
                        console.log("ws connection closed..");
                        reject();
                    });
                });
            }
        });
};

Game.prototype.initShapes = function() {
    var game = this;
    return serverRequest("shapes")
        .then(function(response) {
            var shapes = response;
            game.shapes = shapes;
            if (shapes.length == 0) {
                throw new Error("0 shapes received from server!");
            }
            for (var i = 0; i < shapes.length; i++) {
                var shape = shapes[i];
                var rots = shape.rotations;
                // TODO encapsulate
                for (var r = 0; r < rots.length; r++) {
                    var zeroSeen = false;
                    var rot = rots[r];
                    var rotH = rot.height;
                    var rotCoords = rot.configurations;
                    for (var b = 0; b < rotCoords.length; b++) {
                        var cr = rotCoords[b];
                        cr[1] *= -1;
                        cr[1] += rotH - 1;
                        assert(cr[1] >= 0);
                        zeroSeen = zeroSeen || cr[1] == 0;
                    }
                    assert(zeroSeen);

                    CRUSTNAMES = ["top", "bot", "left", "right"];
                    for (var c = 0; c < CRUSTNAMES.length; c++) {
                        var crust = rot.crusts[CRUSTNAMES[c]];
                        for (var b = 0; b < crust.length; b++) {
                            var cr = crust[b];
                            cr[1] *= -1;
                            cr[1] += rotH - 1;
                            assert(cr[1] >= 0);
                        }
                    }
                }
            }
        });
};

Game.prototype.initGameNo = function() {
    return serverRequest("/games").then(function(response) {
        var gamenoList = response;
        if (gamenoList.length == 0) {
            throw new Error("no current games on server");
        } else {
            var gameNo = gamenoList[gamenoList.length - 1];
            console.log("init gameNo is " + this.gameNo);
            return gameNo;
        }
    });
};

Game.prototype.planExecute = function() {
    var game = this;
    return new Promise(function(resolve, reject) {
        game.planExecuteCallback(function() {
            resolve();
        }, reject);
    });
};

Game.prototype.planExecuteCallback = function(resolve, reject) {
    var game = this;
    var b = game.b;
    var answer = game.answer;
    var ui = this.ui;

    this.ui.paintTo(b, UI.prototype.colors.blank); // erase
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
        if (!this.grid.drop(this.b)) {
            game.gameOver = true;
            reject();
        } else {
            this.ui.paintTo(b, ui.colors.filled);
            this.grid.clearLines(this.ui);
            setTimeout(resolve, this.timerDelay);
        }
        return;
    }

    // check if move is possible
    if (this.grid.blockIntersects(b, ui)) {
        // undo last move and repaint
        b.r = origR;
        b.x = origX;
        this.paintTo(b, UI.prototype.colors.filled);
        game.gameOver = true;
        reject();
    } else {
        this.ui.paintTo(b, UI.prototype.colors.filled);
        setTimeout(this.planExecuteCallback.bind(this, resolve, reject), this.timerDelay);
    }
};

Game.prototype.pauseToggle = function() {
    this.pausedP = !pauseP;
};

Game.prototype.gameOver = function() {
    alert("game over!");
};

Game.prototype.fetchPlanExecuteLoop = function() {
    var game = this;
    this.fetch()
        .then((function() {
            game.ui.paintTo(game.b, UI.prototype.colors.filled);
        }).bind(this))
        .then(this.planExecute.bind(this))
        .then(this.fetchPlanExecuteLoop.bind(this))
        .catch(handleError);
};

Game.prototype.start = function() {
    var game = this;
    this.initGameNo()
        .then(this.init.bind(this))
        .then(this.initShapes.bind(this))
        .then(this.fetchPlanExecuteLoop.bind(this));
};

window.onload = function() {
    var parentElt = document.getElementsByTagName("body")[0];
    assert(parentElt);
    new Game(parentElt).start();
};
