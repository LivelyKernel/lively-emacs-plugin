/*

eval $(flatn -C $LIVELY/lively.next-node_modules \
  -D $LIVELY/lively.modules -D $LIVELY/lively.2lively \
  -D $LIVELY/lively-system-interface -C ./deps env) && node

curl http://10.0.1.3:9011/lively.2lively/dist/lively.2lively_client.js >
lively.2lively_client.js curl
http://10.0.1.3:9011/lively.modules/dist/lively.modules.js > lively.modules.js
curl
http://10.0.1.3:9011/lively-system-interface/dist/lively-system-interface-only-local.js
> lively-system-interface-only-local.js

*/


global.io = require('socket.io-client');
global.babel = require('babel-standalone');
require('systemjs');
require('./lively.modules.js')
// require('./lively-system-interface-only-local.js')
require('./lively-system-interface.js')
require('./lively.2lively_client.js');

let url = `http://localhost:9011/lively-socket.io`;
lively.l2l.client = lively.l2l.L2LClient.ensure(
    {url, namespace: 'l2l', info: {type: 'l2l from node repl windows'}});
lively.l2l.client.whenRegistered(20 * 1000)
    .then(() => console.log('[l2l] online'))
    .catch(err => console.error('[l2l] failed:', err));
