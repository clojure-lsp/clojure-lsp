package clojure_lsp;

import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4j.services.LanguageClient;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;

import clojure_lsp.feature.test_tree.TestTreeParams;

/**
 * Interface for extra methods/notifications for the LSP client
 *
 * @author Eric Dallo
 *
 */
public interface ClojureLanguageClient extends LanguageClient {

    @JsonNotification("clojure/textDocument/testTree")
    void publishTestTree(TestTreeParams testTreeParams);

}
