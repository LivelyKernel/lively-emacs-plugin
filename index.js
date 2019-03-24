/*global require,global,process,lively*/

const {readFileSync} = require('fs');

function loadDependencies() {
  global.io = require('socket.io-client');
  global.babel = require('babel-standalone');
  require('systemjs');
  require('./lively.modules.js')
  require('./lively-system-interface.js')
  require('./lively.2lively_client.js');
}

function readVersion() {
  return JSON.parse(readFileSync(`${__dirname}/package.json`)).version;
}

function connect() {
  let url = `http://localhost:9011/lively-socket.io`;
  let client = lively.l2l.client = lively.l2l.L2LClient.ensure(
      {url, namespace: 'l2l', info: {type: 'l2l emacs', location: 'inferior emacs'}});
  return client.whenRegistered(20 * 1000).then(() => {
    client.info.id = client.id;
    return client;
  });
}

function setupStdinReader() {
  process.stdin.on('data', async data => {
    try {
      // appendFileSync('/tmp/out.log', `received ${data}`);

      const cmd = JSON.parse(data.toString());
      const result = await processCommand(cmd);
      let resultJson;
      try {
        resultJson = JSON.stringify({result});
      } catch (err) {
        resultJson = JSON.stringify({error: `Error stringifing result`})
      }

      process.stdout.write(resultJson + '\n');
    } catch (err) {
      const error = {error: `Failed to process command: ${err.message || err}`};
      process.stdout.write(JSON.stringify(error));
      // console.error(err);
      // appendFileSync('/tmp/out.log', String(err));
    }
  });

  process.stdin.on('error', (err) => {
    // appendFileSync('/tmp/out.log', String(err));
    console.error('process error', err);
    process.exit(2);
  });
}

const commands = {

  runEval: async cmd => {
    const system = cmd.peerId
      ? lively.systemInterface.l2lInterfaceFor(cmd.peerId, lively.l2l.client)
      : lively.systemInterface.localInterface;
    const env = {
      format: 'esm',
      targetModule: null,
      context: {},
      sourceURL: '_emacs_doit_' + Date.now()
    };
    return system.runEval(cmd.source, env);
  },

  listPeers: async _cmd => {
    const result = await lively.l2l.client.listPeers();
    if (result.error) {
      throw result.error;
    }
    return [lively.l2l.client.info].concat(result);
  },
};

async function processCommand(cmd) {
  const handler = commands[cmd.action];
  if (typeof handler == "function") {
    return handler(cmd);
  }
  throw new Error(`Unknown command ${JSON.stringify(cmd)}`);
}

async function main() {
  try {
    loadDependencies();
    await connect();
    setupStdinReader();
    console.log(`lively.emacs started, version ${readVersion()}`);
  } catch (err) {
    console.error('[l2l] failed:', err);
  }
}

main();
