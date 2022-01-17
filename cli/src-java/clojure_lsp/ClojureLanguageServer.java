package clojure_lsp;

import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.TextDocumentIdentifier;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

import clojure_lsp.feature.cursor_info.CursorInfoParams;
import clojure_lsp.feature.clojuredocs.ClojuredocsParams;

/**
 * Interface for extensions for the LSP protocol
 *
 * @author Eric Dallo
 *
 */
public interface ClojureLanguageServer extends LanguageServer {

    @JsonRequest("clojure/dependencyContents")
    @SuppressWarnings("unchecked")
    default CompletableFuture<String> dependencyContents(TextDocumentIdentifier documentUri) {
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("clojure-lsp.server"));
        IFn extension = Clojure.var("clojure-lsp.server", "extension");
        return (CompletableFuture<String>) extension.invoke("dependencyContents", documentUri);
    }

    @JsonRequest("clojure/serverInfo/raw")
    CompletableFuture<Object> serverInfoRaw();

    @JsonNotification("clojure/serverInfo/log")
    void serverInfoLog();

    @JsonRequest("clojure/cursorInfo/raw")
    CompletableFuture<Object> cursorInfoRaw(CursorInfoParams cursorInfoParams);

    @JsonNotification("clojure/cursorInfo/log")
    void cursorInfoLog(CursorInfoParams cursorInfoParams);

    @JsonRequest("clojure/clojuredocs/raw")
    CompletableFuture<Object> clojuredocsRaw(ClojuredocsParams clojuredocsParams);

}
