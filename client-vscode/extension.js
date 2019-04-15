
const vscode = require('vscode');
const lsp = require('vscode-languageclient');

let jarEventEmitter = new vscode.EventEmitter();
let contentsRequest = new lsp.RequestType('clojure/dependencyContents');function activate(context) {

  let serverOptions =  {
    run : { command: "bash", args: ["-c", "clojure-lsp"]},
    debug: { command: 'bash', args: ['-c', 'cd /Users/case/dev/lsp/ && lein run'] }
  };

  // Options to control the language client
  let clientOptions = {
    // Register the server for plain text documents
    documentSelector: [{scheme: 'file', language: 'clojure'}],
    synchronize: {
      configurationSection: 'clojure-lsp',
      fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
    },
    initializationOptions: {
      "dependency-scheme": "jar"
    }
  };

  let languageClient = new lsp.LanguageClient('clojure-lsp', 'Clojure Language Client', serverOptions, clientOptions)

  context.subscriptions.push(languageClient.start());

  let provider = {
    onDidChange: jarEventEmitter.event,
    provideTextDocumentContent: (uri, token) => {
      return languageClient.sendRequest(contentsRequest, { uri: decodeURIComponent(uri.toString()) }, token).then((v) => {
        return v || '';
      });
    }
  };
  context.subscriptions.push(vscode.workspace.registerTextDocumentContentProvider('jar', provider));
}
exports.activate = activate;

// this method is called when your extension is deactivated
function deactivate() {
}
exports.deactivate = deactivate;
