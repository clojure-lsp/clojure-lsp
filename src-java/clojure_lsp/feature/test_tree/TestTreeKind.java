package clojure_lsp.feature.test_tree;

public enum TestTreeKind {
    Deftest(1), Testing(2);

    private final int value;

    TestTreeKind(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public static TestTreeKind forValue(int value) {
        TestTreeKind[] allValues = TestTreeKind.values();
        if (value < 1 || value > allValues.length)
            throw new IllegalArgumentException("Illegal enum value: " + value);
        return allValues[value - 1];
    }
}
