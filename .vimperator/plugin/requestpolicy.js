(function(){
let control = plugins.requestpolicy = {
  getRejected: function () {
    var url = window.requestpolicy.overlay.getTopLevelDocumentUri(),
      id = window.requestpolicy.overlay.getTopLevelDocumentUriIdentifier(),
      otherOrigins = window.requestpolicy.mod.RequestUtil.getOtherOrigins(content.document),
      x = window.requestpolicy.mod.RequestUtil.getRejectedRequests(url, id, otherOrigins),
      res = [];
    for (var i in x)
      res.push(i);
    return res;
  },
  getAllowed: function () {
    var url = window.requestpolicy.overlay.getTopLevelDocumentUri(),
      id = window.requestpolicy.overlay.getTopLevelDocumentUriIdentifier(),
      otherOrigins = window.requestpolicy.mod.RequestUtil.getOtherOrigins(content.document),
      x = window.requestpolicy.mod.RequestUtil.getAllowedRequests(url, id, otherOrigins),
      res = [];
    for (var i in x)
      res.push(i);
    return res;
  },
  allow: function (host) {
    window.requestpolicy.overlay.allowDestination(host);
  },
  revoke: function (host) {
    window.requestpolicy.overlay.forbidDestination(host);
  },
  temporaryAllow: function (host) {
    window.requestpolicy.overlay.temporarilyAllowDestination(host);
  },
  revokeTemporary: function () {
    window.requestpolicy.overlay.revokeTemporaryPermissions({});
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
commands.addUserCommand(['requestpolicy'], 'RequestPolicy command',
    function (args) {
      return control.execute(args);
    }, {
      completer: function (context, args) {
        [context.title, context.completions] = control.complete(args);
      },
      literal: 1
    });
})();
