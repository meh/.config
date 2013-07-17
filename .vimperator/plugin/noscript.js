(function(){
let control = plugins.noscript = {
  getSite: function(host) {
    return window.noscriptOverlay.ns.getDomain(host);
  },
  isRejected: function(host) {
    if (typeof host == 'string')
      return (window.noscriptOverlay.ns.jsPolicySites.matches(host.toString()).length == 0);
    return false;
  },
  getRejected: function() {
    var sites = window.noscriptOverlay.getSites(),
      res = [];
    for (var i = 0; i < sites.length; i++) {
      var site = this.getSite(sites[i]);
      if (typeof site == 'string' && site.length > 0 && this.isRejected(site))
        res.push(site);
    }
    return res;
  },
  getAllowed: function() {
    var sites = window.noscriptOverlay.getSites(),
      res = [];
    for (var i = 0; i < sites.length; i++) {
      var site = this.getSite(sites[i]);
      if (typeof site == 'string' && site.length > 0 && !this.isRejected(site))
        res.push(site);
    }
    return res;
  },
  allow: function(host) {
    window.noscriptOverlay.safeAllow(host, true);
  },
  revoke: function(host) {
    window.noscriptOverlay.safeAllow(host, false);
  },
  temporaryAllow: function(host) {
    window.noscriptOverlay.safeAllow(host, true, true);
  },
  revokeTemporary: function() {
    window.noscriptOverlay.revokeTemp();
  },
  complete: function(args) {
    if (args.completeArg == 0) {
      return [['command', 'description'], [
        ['allow', 'allow host'],
        ['revoke', 'revoke host'],
        ['allow-tmp', 'allow host temporary'],
        ['revoke-tmp', 'revoke all temprary allowed hosts']
      ]];
    } else if (args.completeArg == 1) {
      switch (args[0]) {
        case 'allow':
          var rejected = control.getRejected();
          return [['host'], [[rejected[i], ''] for (i in rejected)]];
          break;
        case 'revoke':
          var allowed = control.getAllowed();
          return [['host'], [[allowed[i], ''] for (i in allowed)]];
          break;
        case 'allow-tmp':
          var rejected = control.getRejected();
          return [['host'], [[rejected[i], ''] for (i in rejected)]];
          break;
      }
      return [];
    }
  },
  execute: function (args) {
    switch (args[0]) {
      case 'allow':
        this.allow(args[1]);
        break;
      case 'revoke':
        this.revoke(args[1]);
        break;
      case 'allow-tmp':
        this.temporaryAllow(args[1]);
        break;
      case 'revoke-tmp':
        this.revokeTemporary();
        break;
    }
  }
};
commands.addUserCommand(['noscript'], 'noscript command',
    function (args) {
      return control.execute(args);
    }, {
      completer: function (context, args) {
        [context.title, context.completions] = control.complete(args);
      },
      literal: 1
    });
})();
