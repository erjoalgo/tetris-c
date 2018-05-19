// var ws_url = "ws://ec2-34-214-135-69.us-west-2.compute.amazonaws.com:81/games/1";

function wsPerf(ws_url, max, tries) {
    if (max == null) {
        max = 10000;
    }
    if (tries == null) {
        tries = 1;
    }
    console.log("starting perf test: " + tries);

    var ws = new WebSocket(ws_url);
    var moveNo = 0;
    var start;
    ws.addEventListener('message', function(event) {
        if (moveNo == max) {
            var elapsed = (window.performance.now() - start) / 1000;
            console.log("finished. total secs: " + precisionRound(elapsed, 2) +
                " moves/sec: " + precisionRound(max / elapsed, 2));
            if (--tries != 0) { // allow infinite testing with input tries = 0
                wsPerf(ws_url, max, tries);
            }
        } else {
            ws.send(moveNo++);
        }
    });

    ws.addEventListener('open', function(event) {
        start = window.performance.now();
        ws.send(0);
    });
}
