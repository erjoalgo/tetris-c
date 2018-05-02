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

var square = "30";
var cell_grid = [];
var retry_timeout = 500;
// function hola{return 1;}
var loading;

function hide_show_loading ( show )
{
    // loading.style=show?"":;
    if (show)
    {
	loading.height = "400";
	loading.width = "550";
	loading.style="";
    }
    else
    {
	loading.height = "0";
	loading.width = "0";
	loading.style="visibility:hidden";

    }
}
function table_create (width, height) {

    var body = document.getElementsByTagName("body")[0];

    loading = document.createElement("img");
    loading.src = "/loading.gif";

    hide_show_loading(false);
    body.appendChild(loading);

    var tbl     = document.createElement("table");
    tbl.class = "table";
    var tblBody = document.createElement("tbody");

    for (var j = 0; j < height; j++) {
	cell_row = [];
	cell_grid.push(cell_row);
        var row = document.createElement("tr");
        for (var i = 0; i < width; i++) {
            var cell = document.createElement("td");
	    cell_row.push(cell);

	    cell.width = square;
	    cell.height = square;
	    cell.bgColor = blank_color;
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
    completed = true;
}

var consec_failed_mills = 0;
var server_timeout = 20000;
var timer_delay = 90;



function assert(condition, message) {

    // todo: upload stack trace

    if (!condition)
    {
	message = message || "Assertion failed"
        alert(message);
	debugger;
        throw message;
    }
}

function server_request ( requestcode, response_hanlder )
{

    assert(requestcode!=null && response_hanlder != null);

    var xhr = new XMLHttpRequest();
    xhr.open('get', requestcode);
    xhr.onreadystatechange = function(){
	// Ready state 4 means the request is done
	if(xhr.readyState === 4){
            if(xhr.status != 200)
            {
		error(xhr.status+": "+xhr.responseText);
            }else
	    {
		var response = null;
		try{
		    response = JSON.parse(xhr.responseText);
		}
		catch (err)
		{
		    console.log("failed to parse request: " + xhr.responseText);

		    consec_failed_mills+=retry_timeout;

		    if (consec_failed_mills>server_timeout)
		    {
			error("server seems unresponsive. try again later")
		    }
		    else
		    {
			setTimeout(function(){server_request(requestcode, response_handler)},retry_timeout);
		    }
		    return;
		}
		assert(! (response==null), " error from server");
		consec_failed_mills = 0;
		response_hanlder(response);
            }
	}
    }
    xhr.send(null);
}

var colors = function(){return {
    'BLUE':"#0000f0",
    'BLACK':"#000000",
    'WHITE':"#ffffff",
    'GREEN':3
    }}();

var filled_color = colors.BLUE;
var blank_color = colors.WHITE;

var state = {
    move_queue:[],
    b:{ //active block
        m:null,//model number
        r:null,//rotation state
        x:null,//x distance from left
        y:null//y from top
    },
    answer:{ //server game move response
        r:null,
        x:null
    },
    paused_p:false,
    game_over:false,
    move_no:null,
    game_no:null,
    shapes:null
}

var grid = {
    relief:null,
    width:null,
    height:null,
    need_clear:[],
    needs_clear:false,
    rowcounts:null,
    grid:null
};

function virtual_iterate ()
{
    // TODO re-do this
    var b = state.b;
    var rot_coords = state.shapes[b.m].rotations[b.r].configurations;
    var coords = [];
    for (var i = 0; i<rot_coords.length;i++)
    {
	coords.push([rot_coords[i][0]+b.x, rot_coords[i][1]+b.y]);
    }
    return coords;
}

function grid_block_intersects (  )    {
    var coords = virtual_iterate();
    for (var i = 0;i< coords.length; i++)
    {
	xy = coords[i];
	if (cell_grid[xy[1]][xy[0]].bgColor!=blank_color)
	{
            return true;
	}

    }
    return false;
}
function paint_to (color, check_intersects)
{
    var xy;
    if (check_intersects && grid_block_intersects())
    {
        return true;
    }else     {
        var coords = virtual_iterate();
        for (var i = 0;i< coords.length; i++)
        {
	    xy = coords[i];
	    cell_grid[xy[1]][xy[0]].bgColor = color;
	    cell_grid[xy[1]][xy[0]].bgColor = color;
        }
        return true;
    }
}

function move_tetro ( move_fun )
{


    paint_to(blank_color);
    move_fun();
    var succ = paint_to(filled_color, true);//undo last move and repaint if this doesn't succeed
    if (!succ)
    {
        state.game_over = true;
	move_fun(true);//undo
	paint_to(filled_color);
    }

}

function add_tetro (  )
{
    paint_to(filled_color);
}


function left ( undo ) {
    !undo?state.b.x--:state.b.x++;
}

function right ( undo ) {
    !undo?state.b.x++:state.b.x--;
}

function rotcw ( undo ) {
    !undo?state.b.r++:state.b.r--;
    state.b.r%=4;
}

function rotccw ( undo ) {
    !undo?state.b.r--:state.b.r++;
}

function down ( undo ) {
    !undo?state.b.y++:state.b.y--;
}


function get_drop_distance (  )
{
    var b = state.b;
    var bot_crust = state.shapes[b.m].rotations[b.r].crusts["bot"];

    var dist, min_dist = grid.height;
    var x = state.b.x;
    var y = state.b.y;

    for (var i = 0, relief_y; i<bot_crust.length; i++)
    {
	xy = bot_crust[i];
	relief_y = grid.relief[xy[0] +b.x];
	dist = relief_y - xy[1];
	if (dist<min_dist)
	{
	    min_dist = dist;
	    new_y = relief_y;
	}
    }

    var drop_dist = min_dist-1-b.y;
    return drop_dist;
}

function drop (  )
{
    var drop_distance  = get_drop_distance();
    if (drop_distance<0)
    {
	state.game_over = true;
    }else     {

        var b = state.b;
        b.y += drop_distance;

        var top_crust = state.shapes[b.m].rotations[b.r].crusts["top"];
        for (var i = 0, xy = null; i<top_crust.length; i++)
        {
	    xy = top_crust[i];
	    grid.relief[xy[0] + b.x] = xy[1]+b.y;
        }

        var coords = virtual_iterate();
        for (var i = 0;i< coords.length; i++)
	{
	    xy = coords[i];

	    if (++grid.rowcounts[xy[1]]==grid.width)
	    {
		grid.needs_clear = true;
		grid.need_clear.push(xy[1]);
	    }
	    grid.grid[xy[1]][xy[0]] = filled_color;
	}
    }
}

function list_min ( list )
{
    var min = null;
    for (var i = 0; i<list.length;i++ )
    {
	if (min==null || list[i]<min)
	    min = list[i];
    }
    return min;
}


function clear_lines (  )
{
    if (!grid.needs_clear) return ;

    var cmpNum = function(a,b){return a-b}
    // grid.last_cleared = grid.need_clear.length;
    if (grid.need_clear.length>0)	{
	// cmpNum is necessary, otherwise sort is lexicographic, eg 10<9
	// would be nice to make need_clear a pqueue
	grid.need_clear.sort (cmpNum);//smallest to largest
    }
    const YMIN = grid.need_clear[grid.need_clear.length-1];
    const YMAX = list_min(grid.relief);//the tallest row is 0. ymax should be as small as possible
    var y = YMIN;
    // grid.need_clear.reverse ();
    var nextNonFull = y-1;//not necessarily non-full here
    var cleared = [];

    while (nextNonFull >= YMAX)
    {
	while (grid.rowcounts[nextNonFull] == grid.width)
	    nextNonFull-=1;
        if (nextNonFull < YMAX)
            break;
	//nextNonFull should be non-full now
        if (grid.rowcounts[y]==grid.width)
	{
            assert(grid.need_clear[grid.need_clear.length-1] == y, " assertion failed at 485 ");
            grid.need_clear.pop ();
            cleared.push(grid.grid[y]);
	}
	//copy nextNonFull row into y-th row
        grid.grid[y] = grid.grid[nextNonFull];
        grid.rowcounts[y] = grid.rowcounts[nextNonFull];
        y-=1;
        nextNonFull -=1;
	}

    while (grid.need_clear.length>0)
        cleared.push(grid.grid[grid.need_clear.pop ()]);

    while (y>=YMAX)
    {
        // assert((cleared.length>0  && sum(cleared[0])==grid.width)
            // || sum(grid.grid[y])==grid.width);

        assert(cleared.length> 0,  " cleared.length assertion ");
        grid.grid[y] = cleared.pop ();
        grid.rowcounts[y] = 0;
        for (var i = 0; i<grid.width;i++)
            grid.grid[y][i] = blank_color;
        y-=1;
    }

    assert(grid.need_clear.length==0,  " grid.need_clear.length==0 assertion failed");
    assert(cleared.length==0,  "cleared.length==0 assertion failed");

    for (var i = 0; i<grid.width; i++)
    {
        var relief = grid.relief[i];
        while (relief<grid.height && grid.grid[relief][i] ==blank_color)
	    relief+=1;
        grid.relief[i] = relief;
    }

    grid.needs_clear = false;
    repaint_rows(YMAX, YMIN+1);
}

function repaint_rows ( ymin, ymax )
{
    for (;ymin<ymax;ymin++)
    {
	for (var x = 0; x<grid.width; x++)
	{
	    cell_grid[ymin][x].bgColor = grid.grid[ymin][x];
	}
    }
}

function fetch ( response )
{
    assert(state.game_no != null && state.move_no !=  null);

    if (response==null)
    {
        var uri = "/games/"+state.game_no+"/moves/"+state.move_no;
        server_request(uri, fetch);
	return;
    }else
    {
        move = response;

        state.b.m = move.shape, state.b.r = 0, state.b.x = grid.width/2-1, state.b.y = 0;
        state.answer.r = move.rot, state.answer.x = move.col;
        state.move_no++;
        timer();
    }
}

function init ( response )
{
    if (response==null)
    {
	    server_request("/games/"+state.game_no, init);
	    return;
	}

    var miny = grid.height;

    game = response;

    state.move_no = game.move_no;

    // game.move_no is for current move.
    state.move_no++;

    console.log("move no is: " +state.move_no);
    grid.width = game.width;
    grid.height = game.height;

    grid.rowcounts = []
    grid.grid = [];
    grid.relief = [];
    state.answer_rx = [null, null];

    table_create(grid.width, grid.height);//delete previous table

    for (var i = 0; i<grid.height;i++)
    {
	grid.rowcounts.push(0);
	var row = [];
	grid.grid.push(row);
	for (var ii = 0; ii<grid.height;ii++)
	    row.push(blank_color);
    }
    for (var i = 0; i<grid.width;i++)
    {
	grid.relief.push(grid.height);
    }

    for (var i = 0;i<game.on_cells.length;i++)
    {
        xy = game.on_cells[i];
        x = xy%grid.width;
        y = Math.floor(xy/grid.width);
        y = grid.height-1-y;

	grid.grid[y][x] = filled_color;
	cell_grid[y][x].bgColor = filled_color;
	if (y<miny)
	    miny = y;
	if (y<grid.relief[x])
	{
	    grid.relief[x] = y;
	}
	grid.rowcounts[y]++;
    }
    repaint_rows(0, miny);
    timer();
}

function init_shapes ( response )
{
    if (response == null)    {
        server_request("shapes", init_shapes)
    }else     {
        state.shapes = response;
        if (state.shapes.length == 0)    {
            error("0 shapes received from server!");
        }
        for (var i = 0; i<state.shapes.length; i++)    {
            var shape = state.shapes[i];
            var rots = shape.rotations;
            for (var r = 0; r<rots.length; r++)    {
                var zero_seen = false;
                var rot = rots[r];
                var rot_h = rot.height;
                var rot_coords = rot.configurations;
                for (var b = 0; b<rot_coords.length; b++)    {
                    var cr = rot_coords[b];
                    cr[1] *= -1;
                    cr[1] += rot_h-1;
                    assert(cr[1]>=0);
                    zero_seen = zero_seen || cr[1] == 0;
                }
                assert(zero_seen);

                CRUST_NAMES = ["top", "bot", "left", "right"];
                for (var c = 0; c<CRUST_NAMES.length; c++)    {
                    var crust = rot.crusts[CRUST_NAMES[c]];
                    for (var b = 0; b<crust.length; b++)    {
                        var cr = crust[b];
                        cr[1]*=-1;
                        cr[1]+=rot_h-1;
                        assert(cr[1]>=0);
                    }
                }
            }
        }
        timer();
    }
}

function error ( message )    {
    var msg = "error: "+message;
    console.log(msg);
    alert(msg);
}

function init_game_no ( response )
{
    if (response == null)    {
        server_request("/games", init_game_no);
    }else     {
        gameno_list = response;
        if (gameno_list.length == 0)    {
            error("no current games on server");
        }else     {
            state.game_no = gameno_list[gameno_list.length-1];
            console.log( "init game_no is "+state.game_no );
            timer();
        }
    }
}

function plan (  )
{
    for (var r  = state.b.r, direc = state.b.r<state.answer.r?1:-1; r!=state.answer.r; r+=direc)
    {
	state.move_queue.push(direc>0?rotcw:rotcw);
    }
    for (var x  = state.b.x, direc = state.b.x<state.answer.x?1:-1; x!=state.answer.x; x+=direc)
    {
	state.move_queue.push(direc>0?right:left);
    }
    state.move_queue.push(drop);
    state.move_queue.push(clear_lines);
    state.move_queue.push(fetch);
    state.move_queue.push(add_tetro);
    state.move_queue.push(plan);
}

function pause_toggle (  )
{
    state.paused_p = !pause_p;
}

function game_over_fun (  )
{

    state.game_over = true;
    //debugger;
    alert("game over!");
    console.log("game over");
}

function timer (  )
{
    if (state.paused_p)
    {
	return ;
    }
    if (state.move_queue.length>0)
    {
	var move = state.move_queue.shift();
	if (move.name in paint_moves)
	{
	    move_tetro(move);
	}
	else
	{
	    move();
	}
	if (state.game_over)
	{
	    //debugger;

	    state.game_over_fun();
	}
	else if (!(move.name in two_step_moves))
	{
	    if ((move.name in no_delay_moves ))
		{
		    timer();
		}

	    else
	    {
                var extra = move.name=="plan"? 200*Math.random() : 0;
		setTimeout(timer,timer_delay+extra);
	    }

	}
	//otherwise two-step-move must bring timer back to life

    }
    else
    {
	alert("no more pending moves");
    }
}

paint_moves = {rotcw:true, rotccw:true, drop:true, left:true, right:true, down:true};
two_step_moves = {fetch:true, init:true, init_shapes:true, init_game_no:true};
no_delay_moves = {fetch:true, init:true};

/*unfortunate hack for IE, in which function.name doesn't work:*/
init.name = "init";
fetch.name = "fetch";
rotcw.name = "rotcw";
rotccw.name = "rotccw";
left.name = "left";
right.name = "right";
drop.name = "drop";
down.name = "down";
init_shapes.name = "init_shapes";
clear_lines.name = "clear_lines";
pause_toggle.name = "pause_toggle";
plan.name = "plan";
add_tetro.name = "add_tetro";

state.move_queue.push(init_game_no);
state.move_queue.push(init);
state.move_queue.push(init_shapes);
state.move_queue.push(fetch);
state.move_queue.push(add_tetro);
state.move_queue.push(plan);

console.log("hola");

timer();
