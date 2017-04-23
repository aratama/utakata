exports.minimize = function(){
    var electron = require('electron');
    var window = electron.remote.getCurrentWindow();
    window.minimize();
};

exports.close = function(){
    var electron = require('electron');
    var window = electron.remote.getCurrentWindow();
    window.close();
};

exports.openDirectoryEff = function(reject){
    return function(resolve){
        return function(){
            var electron = require('electron');
            var dialog = electron.remote.dialog;
            var browserWindow = electron.remote.BrowserWindow;
            var focusedWindow = browserWindow.getFocusedWindow();
            dialog.showOpenDialog(focusedWindow, {
                properties: ["openFile"]
            }, function(directories){
                resolve(typeof directories == "undefined" ? null : directories[0])();
            });
        };
    };
};

exports.home = process.env.HOME;