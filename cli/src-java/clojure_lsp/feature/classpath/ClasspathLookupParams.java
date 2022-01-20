package clojure_lsp.feature.classpath;

import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler;

public class ClasspathLookupParams {

    @NonNull
    private String filepath;

    public String getFilepath() {
        return filepath;
    }

    @Override
    public String toString() {
        return MessageJsonHandler.toString(this);
    }

}
