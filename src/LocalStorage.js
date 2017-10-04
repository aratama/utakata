exports.loadStorage_ = function(key){
    return function(){
        return window.localStorage.getItem(key);
    }
};

exports.saveStorage_ = function(key){
    return function(value){
        return function(){
            window.localStorage.setItem(key, value);
        };
    };
};