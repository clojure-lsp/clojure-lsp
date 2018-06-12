
const vscode = require('vscode');
console.log("WAFASDF")
const lsp = require('vscode-languageclient');

function activate(context) {

	let serverOptions =  {
		run : { command: "clojure-lsp" },
		debug: { command: 'bash', args: ['-c', 'cd /Users/case/dev/lsp/ && lein run'] }
	};

	// Options to control the language client
	let clientOptions = {
		// Register the server for plain text documents
		documentSelector: [{scheme: 'file', language: 'clojure'}],
		synchronize: {
			configurationSection: 'clojure-lsp',
			fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
		}
	};

    let disposable = new lsp.LanguageClient('clojure-lsp', 'Clojure Language Client', serverOptions, clientOptions).start();

    context.subscriptions.push(disposable);
}
exports.activate = activate;

// this method is called when your extension is deactivated
function deactivate() {
}
exports.deactivate = deactivate;
