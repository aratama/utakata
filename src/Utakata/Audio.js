exports.createAudioContext = function(){
    window.AudioContext = window.AudioContext||window.webkitAudioContext;
    return new AudioContext();
};

exports.loadAudioEff = function(path){
    return function(context){
        return function(reject){
            return function(resolve){
                return function(){
                    var fs = require("fs");
                    fs.readFile(path, function(error, nodeBuffer){
                        if (error){
                            reject(error)();
                        }else{
                            context.decodeAudioData(nodeBuffer.buffer, function(audioBuffer) {
                                resolve(audioBuffer)();
                            });
                        }
                    });                
                };
            };
        };
    };
};

exports.play = function(audioBuffer){
    return function(context){
        return function(){
            var source = context.createBufferSource();
            source.buffer = audioBuffer;        
            source.connect(context.destination); 
            source.start(0);
            return source;
        };
    };
};

exports.stop = function(source){
    return function(){
        source.stop(0);
    };
};

exports.currentTime = function(context){
    return function(){
        return context.currentTime;
    };
};

