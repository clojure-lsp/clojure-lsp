package clojure_lsp;

import org.eclipse.lsp4j.TextDocumentIdentifier;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler;

public class CursorInfoParams {

    private TextDocumentIdentifier textDocument;

    @NonNull
    private Position position;

    public TextDocumentIdentifier getTextDocument() {
        return textDocument;
    }

    public Position getPosition() {
        return position;
    }

    @Override
    public String toString() {
        return MessageJsonHandler.toString(this);
    }

}
