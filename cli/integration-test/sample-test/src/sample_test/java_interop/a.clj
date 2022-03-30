(ns sample-test.java-interop.a
  (:import
   (clojure.lang PersistentVector)
   (sample_test.java_interop SampleClass)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn my-function []
  (SampleClass.)
  (PersistentVector.))
