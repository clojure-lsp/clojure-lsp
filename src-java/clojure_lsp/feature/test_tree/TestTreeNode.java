package clojure_lsp.feature.test_tree;

import java.util.List;

import org.eclipse.lsp4j.Range;
import org.eclipse.lsp4j.jsonrpc.validation.NonNull;
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler;

public class TestTreeNode {

    @NonNull
    private String name;

    @NonNull
    private Range range;

    @NonNull
    private Range nameRange;

    @NonNull
    private TestTreeKind kind;

    private List<TestTreeNode> children;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Range getRange() {
        return range;
    }

    public void setRange(Range range) {
        this.range = range;
    }

    public TestTreeKind getKind() {
        return kind;
    }

    public void setKind(TestTreeKind kind) {
        this.kind = kind;
    }

    public Range getNameRange() {
        return nameRange;
    }

    public void setNameRange(Range nameRange) {
        this.nameRange = nameRange;
    }

    public List<TestTreeNode> getChildren() {
        return children;
    }

    public void setChildren(List<TestTreeNode> children) {
        this.children = children;
    }

    @Override
    public String toString() {
        return MessageJsonHandler.toString(this);
    }

}
