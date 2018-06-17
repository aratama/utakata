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
                var fade = context.createGain();
                fade.connect(context.destination);

                var gain = context.createGain();
                gain.connect(fade);

                var source = context.createBufferSource();
                source.buffer = audioBuffer;        
                source.connect(gain); 
                source.start(0, offset);

                fade.gain.value = 0;
                fade.gain.linearRampToValueAtTime(1, context.currentTime + 0.05);

                return { context: context, source: source, gain: gain, fade: fade };
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
        graph.fade.gain.linearRampToValueAtTime(0, graph.context.currentTime + 0.05);
        setTimeout(function(){
            graph.source.stop(0);
        }, 50);
    };
};

exports.currentTime = function(context){
    return function(){
        return context.currentTime;
    };
};

exports.getDuration = function(buffer){
    return buffer.duration;
};