package clojure_lsp.feature.test_tree;

import java.util.List;

import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler;

public class TestTreeParams {

    @NonNull
    private String uri;

    @NonNull
    private TestTreeNode tree;

    public String getUri() {
        return uri;
    }

    public void setUri(String uri) {
        this.uri = uri;
    }

    public TestTreeNode getTree() {
        return tree;
    }

    public void setTree(TestTreeNode tree) {
        this.tree = tree;
    }

    @Override
    public String toString() {
        return MessageJsonHandler.toString(this);
    }

}
