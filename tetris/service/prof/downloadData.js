// https://stackoverflow.com/questions/24898044/is-possible-to-save-javascript-variable-as-file

var textToSave = JSON.stringify(state.ws.times);
var hiddenElement = document.createElement('a');

hiddenElement.href = 'data:attachment/text,' + encodeURI(textToSave);
hiddenElement.target = '_blank';
hiddenElement.download = 'myFile.txt';
hiddenElement.click();
