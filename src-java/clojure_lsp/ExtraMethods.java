package clojure_lsp;

import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest;

/**
 * Interface for extensions for the LSP protocol
 *
 * @author Eric Dallo
 *
 */
public interface ExtraMethods {

    @JsonRequest("clojure/serverInfo/raw")
    CompletableFuture<Object> serverInfoRaw();

    @JsonNotification("clojure/serverInfo/log")
    void serverInfoLog();

    @JsonNotification("clojure/cursorInfo/log")
    void cursorInfoLog(CursorInfoParams cursorInfoParams);

    @JsonRequest("clojure/clojuredocs/raw")
    CompletableFuture<Object> clojuredocsRaw(ClojuredocsParams clojuredocsParams);

}
