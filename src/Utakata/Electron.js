exports.close = function(){
    var electron = require('electron');
    var window = electron.remote.getCurrentWindow();
    window.close();
};