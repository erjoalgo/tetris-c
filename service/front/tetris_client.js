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

function createElementWithProperties(tag, props) {
    var elm = document.createElement(tag);
    for (var key in props) {
        // elm.setAttribute(key, attrs[key]);
        var val = props[key];
        var path = key.split(".");
        var last = path.pop();
        var nested = path.reduce(function(cum, a) {
            return cum[a]
        }, elm)
        nested[last] = val;
    }
    return elm;
}

var ui = {
    cellSize: "30",
    cellGrid: [],
    fontSize: "30px",
    loading: createElementWithProperties("img", {
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
    }),
    moveNoElm: null,
    colors: {
        'BLUE': "#0000f0",
        'BLACK': "#000000",
        'WHITE': "#ffffff",
        'GREEN': 3,
        filled: this.BLUE,
        blank: this.WHITE
    },
    tableCreate: function(width, height) {

        var body = document.getElementsByTagName("body")[0];

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
                cell.style.border = "1px solid #000"

                var cellText = document.createTextNode("");
                cell.appendChild(cellText);
                row.appendChild(cell);
            }

            tblBody.appendChild(row);
        }
        tbl.appendChild(tblBody);
        body.appendChild(tbl);
        tbl.setAttribute("border", "2");

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
                min: 1,
                max: 100,
                value: timerDelay,
                invertValue: function(val) {
                    return parseInt(this.max) - val + parseInt(this.min);
                },
                onchange: function() {
                    timerDelay = this.invertValue(this.value);
                }
            });

        this.slider.value = this.slider.invertValue(timerDelay);

        body.appendChild(this.slider);
    },
    paint: function(r, c, color) {
        assert(color != null);
        this.cellGrid[r][c].bgColor = color;
    }
}

ui.colors.filled = ui.colors.BLUE;
ui.colors.blank = ui.colors.WHITE;

var RETRY_TIMEOUT = 500;
var SERVER_TIMEOUT = 20000;
var timerDelay = 90;

function assert(condition, message) {

    // todo: upload stack trace

    if (!condition) {
        message = message || "Assertion failed"
        alert(message);
        debugger;
        throw message;
    }
}

function serverRequest(url, handler) {

    assert(url != null && handler != null);

    var xhr = new XMLHttpRequest();
    xhr.open('get', url);
    xhr.onreadystatechange = function() {
        // Ready state 4 means the request is done
        if (xhr.readyState === 4) {
            if (xhr.status != 200) {
                error(url + " returned non-200 status: " + xhr.status + ": server response: " + xhr.responseText);
            } else {
                var response = null;
                try {
                    response = JSON.parse(xhr.responseText);
                } catch (err) {
                    console.log("failed to parse request: " + xhr.responseText);

                    state.consecFailedMills += RETRY_TIMEOUT;

                    if (state.consecFailedMills > SERVER_TIMEOUT) {
                        error("server seems unresponsive. try again later")
                    } else {
                        setTimeout(function() {
                            serverRequest(url, handler)
                        }, RETRY_TIMEOUT);
                    }
                    return;
                }
                assert(!(response == null), " error from server");
                state.consecFailedMills = 0;
                handler(response);
            }
        }
    }
    xhr.send(null);
}

var state = {
    moveQueue: [],
    b: { //active block
        m: null, //model number
        r: null, //rotation state
        x: null, //x distance from left
        y: null //y from top
    },
    answer: { //server game move response
        r: null,
        x: null
    },
    pausedP: false,
    gameOver: false,
    moveNo: null,
    gameNo: null,
    shapes: null,
    consecFailedMills: 0,

    grid: {
        relief: null,
        width: null,
        height: null,
        needClear: [],
        needsClear: false,
        rowcounts: null,
        g: null
    }
}

function bIter() {
    return (function() {
        var i = 0;

        var x = state.b.x;
        var y = state.b.y;
        var rotCoords = state.shapes[state.b.m].rotations[state.b.r].configurations;

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
        }
    })();
}

function gridBlockIntersects() {
    for (var itr = bIter(); itr.hasNext();) {
        var xy = itr.next().value;
        assert(ui.cellGrid[xy[1]][xy[0]].bgColor == ui.colors.blank ||
            state.grid.g[xy[1]][xy[0]] == ui.colors.filled);
        if (ui.cellGrid[xy[1]][xy[0]].bgColor != ui.colors.blank) {
            return true;
        }
    }
    return false;
}

function paintTo(color, checkIntersects) {
    if (checkIntersects && gridBlockIntersects()) {
        return true;
    } else {
        for (var itr = bIter(); itr.hasNext();) {
            var xy = itr.next().value;
            ui.paint(xy[1], xy[0], color);
        }
        return true;
    }
}

function moveTetro(moveFun) {
    paintTo(ui.colors.blank);
    moveFun();
    var succ = paintTo(ui.colors.filled, true); //undo last move and repaint if this doesn't succeed
    if (!succ) {
        state.gameOver = true;
        moveFun(true); //undo
        paintTo(ui.colors.filled);
    }
}

function addTetro() {
    paintTo(ui.colors.filled);
}


function left(undo) {
    !undo ? state.b.x-- : state.b.x++;
}

function right(undo) {
    !undo ? state.b.x++ : state.b.x--;
}

function rotcw(undo) {
    !undo ? state.b.r++ : state.b.r--;
    state.b.r %= 4;
}

function rotccw(undo) {
    !undo ? state.b.r-- : state.b.r++;
}

function down(undo) {
    !undo ? state.b.y++ : state.b.y--;
}

function getDropDistance() {
    var b = state.b;
    var botCrust = state.shapes[b.m].rotations[b.r].crusts["bot"];

    var grid = state.grid;
    var dist, minDist = grid.height;
    var x = state.b.x;
    var y = state.b.y;

    for (var i = 0, reliefY; i < botCrust.length; i++) {
        xy = botCrust[i];
        reliefY = grid.relief[xy[0] + b.x];
        dist = reliefY - xy[1];
        if (dist < minDist) {
            minDist = dist;
            newY = reliefY;
        }
    }

    var dropDist = minDist - 1 - b.y;
    return dropDist;
}

function drop() {
    var grid = state.grid;
    var dropDistance = getDropDistance();
    if (dropDistance < 0) {
        state.gameOver = true;
    } else {

        var b = state.b;
        b.y += dropDistance;

        var topCrust = state.shapes[b.m].rotations[b.r].crusts["top"];
        for (var i = 0, xy = null; i < topCrust.length; i++) {
            xy = topCrust[i];
            grid.relief[xy[0] + b.x] = xy[1] + b.y;
        }

        for (var itr = bIter(); itr.hasNext();) {
            var xy = itr.next().value;
            if (++grid.rowcounts[xy[1]] == grid.width) {
                grid.needsClear = true;
                grid.needClear.push(xy[1]);
            }
            grid.g[xy[1]][xy[0]] = ui.colors.filled;
        }
    }
}

function listMin(list) {
    var min = null;
    for (var i = 0; i < list.length; i++) {
        if (min == null || list[i] < min)
            min = list[i];
    }
    return min;
}

function clearLines() {
    var grid = state.grid;
    if (!grid.needsClear) return;

    var cmpNum = function(a, b) {
        return a - b
    }
    // grid.lastCleared = grid.needClear.length;
    if (grid.needClear.length > 0) {
        // cmpNum is necessary, otherwise sort is lexicographic, eg 10<9
        // would be nice to make needClear a pqueue
        grid.needClear.sort(cmpNum); //smallest to largest
    }
    const YMIN = grid.needClear[grid.needClear.length - 1];
    const YMAX = listMin(grid.relief); //the tallest row is 0. ymax should be as small as possible
    var y = YMIN;
    // grid.needClear.reverse ();
    var nextNonFull = y - 1; //not necessarily non-full here
    var cleared = [];

    while (nextNonFull >= YMAX) {
        while (grid.rowcounts[nextNonFull] == grid.width)
            nextNonFull -= 1;
        if (nextNonFull < YMAX)
            break;
        //nextNonFull should be non-full now
        if (grid.rowcounts[y] == grid.width) {
            assert(grid.needClear[grid.needClear.length - 1] == y, " assertion failed at 485 ");
            grid.needClear.pop();
            cleared.push(grid.g[y]);
        }
        //copy nextNonFull row into y-th row
        grid.g[y] = grid.g[nextNonFull];
        grid.rowcounts[y] = grid.rowcounts[nextNonFull];
        y -= 1;
        nextNonFull -= 1;
    }

    while (grid.needClear.length > 0)
        cleared.push(grid.g[grid.needClear.pop()]);

    while (y >= YMAX) {
        // assert((cleared.length>0  && sum(cleared[0])==grid.width)
        // || sum(grid.g[y])==grid.width);

        assert(cleared.length > 0, " cleared.length assertion ");
        grid.g[y] = cleared.pop();
        grid.rowcounts[y] = 0;
        for (var i = 0; i < grid.width; i++)
            grid.g[y][i] = ui.colors.blank;
        y -= 1;
    }

    assert(grid.needClear.length == 0, " grid.needClear.length==0 assertion failed");
    assert(cleared.length == 0, "cleared.length==0 assertion failed");

    for (var i = 0; i < grid.width; i++) {
        var relief = grid.relief[i];
        while (relief < grid.height && grid.g[relief][i] == ui.colors.blank)
            relief += 1;
        grid.relief[i] = relief;
    }

    grid.needsClear = false;
    repaintRows(YMAX, YMIN + 1);
}

function repaintRows(ymin, ymax) {
    var grid = state.grid;
    for (; ymin < ymax; ymin++) {
        for (var x = 0; x < grid.width; x++) {
            ui.paint(ymin, x, grid.g[ymin][x] || ui.colors.blank);
        }
    }
}

function fetch(response) {
    assert(state.gameNo != null && state.moveNo != null);

    if (response == null) {
        if (state.ws != null)    {
            state.ws.send(state.moveNo);
        }else     {
            var uri = "/games/" + state.gameNo + "/moves/" + state.moveNo;
            serverRequest(uri, fetch);
        }
        return;
    } else {
        move = response;

        state.b.m = move.shape, state.b.r = 0, state.b.x = state.grid.width / 2 - 1, state.b.y = 0;
        state.answer.r = move.rot, state.answer.x = move.col;
        state.moveNo++;
        ui.moveNoElm.innerHTML = state.moveNo;
        timer();
    }
}

function init(response) {
    if (response == null) {
        serverRequest("/games/" + state.gameNo, init);
        return;
    }
    var grid = state.grid;

    var miny = grid.height;

    game = response;

    var skip = false; // start from move 0`
    if (skip) {
        game.move_no = -1;
        game.on_cells = [];
    }

    state.moveNo = game.move_no;

    // game.moveNo is for current move.
    state.moveNo++;

    console.log("move no is: " + state.moveNo);

    if (game.ws_port)    {
        var ws_url = "ws://" + window.location.hostname + ":" + game.ws_port
            + "/games/" + state.gameNo;
        console.log( "using ws url: " + ws_url );
        state.ws = new WebSocket(ws_url);
        state.ws.addEventListener('message', function (event) {
            move = JSON.parse(event.data); // TODO unpack
            fetch(move);
        });
    }

    grid.width = game.width;
    grid.height = game.height;

    grid.rowcounts = []
    grid.g = [];
    grid.relief = [];
    state.answerRx = [null, null];

    ui.tableCreate(grid.width, grid.height); //delete previous table

    for (var i = 0; i < grid.height; i++) {
        grid.rowcounts.push(0);
        var row = [];
        grid.g.push(row);
        for (var ii = 0; ii < grid.height; ii++)
            row.push(ui.colors.blank);
    }
    for (var i = 0; i < grid.width; i++) {
        grid.relief.push(grid.height);
    }

    for (var i = 0; i < game.on_cells.length; i++) {
        xy = game.on_cells[i];
        x = xy % grid.width;
        y = Math.floor(xy / grid.width);
        y = grid.height - 1 - y;

        grid.g[y][x] = ui.colors.filled;
        ui.paint(y, x, ui.colors.filled);
        if (y < miny)
            miny = y;
        if (y < grid.relief[x]) {
            grid.relief[x] = y;
        }
        grid.rowcounts[y]++;
    }
    repaintRows(0, miny);
    if (state.ws)    {
        state.ws.addEventListener('open', function (event) {
            console.log( "ws connection opened.." );
            timer();
        });
    }else     {
        timer();
    }
}

function initShapes(response) {
    if (response == null) {
        serverRequest("shapes", initShapes)
    } else {
        state.shapes = response;
        if (state.shapes.length == 0) {
            error("0 shapes received from server!");
        }
        for (var i = 0; i < state.shapes.length; i++) {
            var shape = state.shapes[i];
            var rots = shape.rotations;
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
        timer();
    }
}

function error(message) {
    console.log(new Error().stack);
    var msg = "error: " + message;
    console.log(msg);
    alert(msg);
}

function initGameNo(response) {
    if (response == null) {
        serverRequest("/games", initGameNo);
    } else {
        gamenoList = response;
        if (gamenoList.length == 0) {
            error("no current games on server");
        } else {
            state.gameNo = gamenoList[gamenoList.length - 1];
            console.log("init gameNo is " + state.gameNo);
            timer();
        }
    }
}

function plan() {
    for (var r = state.b.r, direc = state.b.r < state.answer.r ? 1 : -1; r != state.answer.r; r += direc) {
        state.moveQueue.push(direc > 0 ? rotcw : rotcw);
    }
    for (var x = state.b.x, direc = state.b.x < state.answer.x ? 1 : -1; x != state.answer.x; x += direc) {
        state.moveQueue.push(direc > 0 ? right : left);
    }
    state.moveQueue.push(drop);
    state.moveQueue.push(clearLines);
    state.moveQueue.push(fetch);
    state.moveQueue.push(addTetro);
    state.moveQueue.push(plan);
}

function pauseToggle() {
    state.pausedP = !pauseP;
}

function gameOverFun() {
    alert("game over!");
    console.log("game over");
}

function timer() {
    if (state.pausedP) {
        return;
    }
    if (state.moveQueue.length > 0) {
        var move = state.moveQueue.shift();
        if (move.name in paintMoves) {
            moveTetro(move);
        } else {
            move();
        }
        if (state.gameOver) {
            gameOverFun();
        } else if (!(move.name in twoStepMoves)) {
            if ((move.name in noDelayMoves)) {
                timer();
            } else {
                var extra = move.name == "plan" ? 200 * Math.random() : 0;
                setTimeout(timer, timerDelay + extra);
            }

        }
        //otherwise two-step-move must bring timer back to life

    } else {
        alert("no more pending moves");
    }
}

paintMoves = {
    rotcw: true,
    rotccw: true,
    drop: true,
    left: true,
    right: true,
    down: true
};
twoStepMoves = {
    fetch: true,
    init: true,
    initShapes: true,
    initGameNo: true
};
noDelayMoves = {
    fetch: true,
    init: true
};

/*unfortunate hack for IE, in which function.name doesn't work:*/
init.name = "init";
fetch.name = "fetch";
rotcw.name = "rotcw";
rotccw.name = "rotccw";
left.name = "left";
right.name = "right";
drop.name = "drop";
down.name = "down";
initShapes.name = "initShapes";
clearLines.name = "clearLines";
pauseToggle.name = "pauseToggle";
plan.name = "plan";
addTetro.name = "addTetro";

state.moveQueue.push(initGameNo);
state.moveQueue.push(init);
state.moveQueue.push(initShapes);
state.moveQueue.push(fetch);
state.moveQueue.push(addTetro);
state.moveQueue.push(plan);

timer();
