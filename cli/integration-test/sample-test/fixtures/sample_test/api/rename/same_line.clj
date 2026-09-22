(ns sample-test.rename.same-line)

(defn present [x] x)

(defn f [a b] (or (present a) (present b)))
