package clojure_lsp;

import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler;

public class ClojuredocsParams {

    @NonNull
    private String symName;

    private String symNs;

    public String getSymName() {
        return symName;
    }

    public String getSymNs() {
        return symNs;
    }

    @Override
    public String toString() {
        return MessageJsonHandler.toString(this);
    }

}
