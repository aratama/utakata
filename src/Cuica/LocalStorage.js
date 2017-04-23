exports.loadStorage_ = function(key){
    return function(){
        try {
            return JSON.parse(window.localStorage.getItem(key));
        }catch(e){
            return {};
        }
    }
};

exports.saveStorage_ = function(key){
    return function(a){
        return function(){
            window.localStorage.setItem(key, JSON.stringify(a));
        };
    };
};