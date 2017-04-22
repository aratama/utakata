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
        };
    };
};

exports.readMetadataEff = function(path){
    return function(reject){
        return function(resolve){
            return function(){
                var fs = require('fs');
                var mm = require('musicmetadata');
                var readableStream = fs.createReadStream(path);
                var parser = mm(readableStream, function (err, metadata) {
                    if (err){ 
                        reject(err)(); 
                    }else{ 
                        readableStream.close();
                        resolve(metadata)(); 
                    }
                });
            };
        };
    };
};