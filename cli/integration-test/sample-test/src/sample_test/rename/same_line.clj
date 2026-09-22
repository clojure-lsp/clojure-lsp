(ns sample-test.rename.same-line)

(defn non-blank [x] x)

(defn f [a b] (or (non-blank a) (non-blank b)))
