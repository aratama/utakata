exports.requestAnimationFrame = function(callback){
    return function(){
        window.requestAnimationFrame(function(){
            callback();
        });
    };
};