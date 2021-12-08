package clojure_lsp.feature.test_tree;

import java.util.List;

import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler;

public class TestTreeParams {

    @NonNull
    private String uri;

    @NonNull
    private List<TestTreeNode> tests;

    public String getUri() {
        return uri;
    }

    public void setUri(String uri) {
        this.uri = uri;
    }

    public List<TestTreeNode> getTests() {
        return tests;
    }

    public void setTests(List<TestTreeNode> tests) {
        this.tests = tests;
    }

    @Override
    public String toString() {
        return MessageJsonHandler.toString(this);
    }

}
