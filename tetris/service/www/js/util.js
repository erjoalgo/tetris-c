var SERVER_TIMEOUT = 20000;
var RETRY_TIMEOUT = 500;

function serverRequest(url, consecFailedMills) {

    if (consecFailedMills == null)    {
        consecFailedMills = 0;
    }

    assert(url != null);

    return new Promise(function(resolve, reject) {
        var xhr = new XMLHttpRequest();
        xhr.open('get', url);
        xhr.onreadystatechange = function() {
            // Ready state 4 means the request is done
            if (xhr.readyState === 4) {
                if (xhr.status != 200) {
                    reject(url + " returned non-200 status: " + xhr.status +
                           ", server response: " + xhr.responseText);
                } else {
                    var response = null;
                    try {
                        response = JSON.parse(xhr.responseText);
                    } catch (err) {
                        console.log("failed to parse request: " + xhr.responseText);

                        if (consecFailedMills > SERVER_TIMEOUT) {
                            reject("server seems unresponsive. try again later");
                        } else {
                            setTimeout(function() {
                                serverRequest(url, consecFailedMills + RETRY_TIMEOUT)
                                    .then(resolve, reject);
                            }, RETRY_TIMEOUT);
                        }
                        return;
                    }
                    assert(response != null, " error from server");
                    resolve(response);
                }
            }
        };
        xhr.send(null);
    });
}

function createElementWithProperties(tag, props) {
    var elm = document.createElement(tag);
    for (var key in props) {
        if (props.hasOwnProperty(key))    {
            // elm.setAttribute(key, attrs[key]);
            var val = props[key];
            var path = key.split(".");
            var last = path.pop();
            var nested = path.reduce(function(cum, a) {
                return cum[a];
            }, elm);
            nested[last] = val;
        }
    }
    return elm;
}

function precisionRound(number, precision) {
    var factor = Math.pow(10, precision);
    return Math.round(number * factor) / factor;
}

function handleError(message) {
    console.log(new Error().stack);
    var msg = "error: " + message;
    console.log(msg);
    alert(msg);
    throw new Error(message);
}

function assert(condition, message) {

    // todo: upload stack trace
    if (!condition) {
        message = message || "Assertion failed";
        alert(message);
        debugger;
        throw message;
    }
}

function listMin(list) {
    var min = list[0];
    for (var i = 0; i < list.length; i++) {
        if (list[i] < min)
            min = list[i];
    }
    return min;
}
