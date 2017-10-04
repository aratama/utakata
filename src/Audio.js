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
                    console.log("loadAudioEff: " + path);
                    fs.readFile(path, function(error, nodeBuffer){
                        if (error){
                            reject(error)();
                        }else{
                            context.decodeAudioData(nodeBuffer.buffer).then(
                                function(audioBuffer) {
                                    resolve(audioBuffer)();
                                }, 
                                function(err){
                                    reject(err)();
                                }
                            );
                        }
                    });                
                };
            };
        };
    };
};

exports.play = function(audioBuffer){
    return function(offset){
        return function(context){
            return function(){
                var gain = context.createGain();
                gain.connect(context.destination);

                var source = context.createBufferSource();
                source.buffer = audioBuffer;        
                source.connect(gain); 
                source.start(0, offset);

                return { source: source, gain: gain };
            };
        };
    };
};

exports.addEndEventListener = function(source){
     return function(onEnd){
        return function(){
            source.onended = function(e){
                onEnd({})();
            };
        };
    };
};

exports.removeEndEventListener = function(source){
    return function(){
        source.onended = null;
    };
};

exports.stop = function(graph){
    return function(){
        graph.source.stop(0);
    };
};

exports.currentTime = function(context){
    return function(){
        return context.currentTime;
    };
};

exports.setGain = function(value){
    return function(graph){
        return function(){
            graph.gain.gain.value = value;
        };
    };
};

exports.getDuration = function(buffer){
    return buffer.duration;
};