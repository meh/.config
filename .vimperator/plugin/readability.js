rdb.vimperator = {
  now: function() {
    rdb.firefox.overlay.read_now({button: 0});
  },
  later: function() {
    rdb.firefox.overlay.read_later({button: 0});
  },
  kindle: function() {
    rdb.firefox.overlay.send_to_kindle({button: 0});
  },
  _execute: function(args) {
    var cmd = rdb.vimperator[args.length ? args.shift() : 'now'];

     if (!cmd) {
       liberator.echoerr('Unsupported readability command: ' + name);
       return false;
     }
     return cmd();
  },
  _completer: function(context, args) {
    args = args.filter(function(c) c != '');
    if (args.length > 0)
      return [0, []];
    return [0, [['now', 'Read Now'], ['later', 'Read Later'], ['kindle', 'Send to Kindle']]];
  }
};

commands.add(['read[ability]'],
    'Control readability from within vimperator.',
    function(args){rdb.vimperator._execute(args)},
    {count: true, argCount: '*', completer: rdb.vimperator._completer}
);

