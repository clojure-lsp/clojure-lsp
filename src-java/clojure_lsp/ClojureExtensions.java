package clojure_lsp;

import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4j.TextDocumentIdentifier;
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest;
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

/**
 * Interface for protocol extensions for Java
 *
 * @author Gorkem Ercan
 *
 */
@JsonSegment("clojure")
public class ClojureExtensions {
    @JsonRequest
    @SuppressWarnings("unchecked")
    CompletableFuture<String> dependencyContents(TextDocumentIdentifier documentUri) {
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("clojure-lsp.server"));
        IFn extension = Clojure.var("clojure-lsp.server", "extension");
        return (CompletableFuture<String>) extension.invoke("dependencyContents", documentUri);
    }
}
