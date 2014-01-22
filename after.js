(function(){
    var exit = function(code) {
        if (!(code === null)) {
            process.exit(code);
        }
    }
    // TODO: Generalize over Elm.ScriptExample :(
    var worker = Elm.worker(Elm.ScriptExample);
    if (worker.ports.exit) {
        worker.ports.exit.subscribe(exit);
    }
})();
