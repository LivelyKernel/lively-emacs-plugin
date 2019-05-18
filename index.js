/*global require,global,process,lively*/

const { readFileSync, appendFileSync } = require("fs");
const debugLogFile = "/tmp/lively-emacs-eval.log";
const debugLogging = false;

function debugLog(msg) {
  debugLogging && appendFileSync(debugLogFile, msg + "\n");
}

function loadDependencies() {
  global.io = require('socket.io-client');
  global.babel = require('babel-standalone');
  require('systemjs');
  require('./lively.modules.js');
  require('./lively-system-interface.js');
  require('./lively.2lively_client.js');
}

function readVersion() {
  return JSON.parse(readFileSync(`${__dirname}/package.json`)).version;
}

function connect(url = "http://localhost:9011/lively-socket.io") {
  debugLog(url);
  let client = (lively.l2l.client = lively.l2l.L2LClient.ensure({
    url,
    namespace: 'l2l',
    info: {type: 'l2l emacs', location: 'inferior emacs'},
  }));
  return client.whenRegistered(20 * 1000).then(() => {
    client.info.id = client.id;
    return client;
  });
}

function setupStdinReader() {
  process.stdin.on('data', async data => {
    try {
      debugLog(`received ${data}`);

      const cmd = JSON.parse(data.toString());
      const result = await processCommand(cmd);
      let resultJson;
      try {
        resultJson = JSON.stringify({result});
      } catch (err) {
        resultJson = JSON.stringify({error: `Error stringifing result`});
      }

      process.stdout.write(resultJson + '\n');
    } catch (err) {
      const error = {error: `Failed to process command: ${err.message || err}`};
      process.stdout.write(JSON.stringify(error) + '\n');
      debugLog(`ERROR when receiving data: ${err.stack}`);
    }
  });

  process.stdin.on('error', err => {
    debugLog(`Process error: ${err.stack}`)
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
      targetModule: cmd.targetModule || "lively://emacs-plugin/dummy-module",
      // context: {},
      sourceURL: '_emacs_doit_' + Date.now(),
      asString: cmd.asString,
      inspect: cmd.inspect,
      inspectDepth: cmd.inspectDepth,
    };
    const result = await system.runEval(cmd.source, env);
    return result;
  },

  completions: cmd => {
    const system = cmd.peerId
      ? lively.systemInterface.l2lInterfaceFor(cmd.peerId, lively.l2l.client)
      : lively.systemInterface.localInterface;
    const env = {
      format: 'esm',
      targetModule: cmd.targetModule,
      context: {},
      sourceURL: '_emacs_completion_' + Date.now(),
    };
    return system.dynamicCompletionsForPrefix(
      env.targetModule,
      cmd.prefix,
      env,
    );
  },

  listPeers: async _cmd => {
    const result = await lively.l2l.client.listPeers();
    if (result.error) {
      throw result.error;
    }
    return result;
    // return [lively.l2l.client.info].concat(result);
  },
};

async function processCommand(cmd) {
  const handler = commands[cmd.action];
  if (typeof handler == 'function') {
    return handler(cmd);
  }
  throw new Error(`Unknown command ${JSON.stringify(cmd)}`);
}

function commandlineArgs() {
  debugLog(process.argv)
  const args = {serverAddress: null};
  const idx = process.argv.indexOf("--server-address");
  if (idx > -1) {
    args.serverAddress = process.argv[idx+1];
  }
  debugLog(JSON.stringify(args))
  return args;
}

async function main() {
  try {
    loadDependencies();
    const args = commandlineArgs();
    await connect(args.serverAddress);
    setupStdinReader();
    console.log(`lively.emacs started, version ${readVersion()}`);
  } catch (err) {
    console.error('[l2l] failed:', err);
  }
}

main();
