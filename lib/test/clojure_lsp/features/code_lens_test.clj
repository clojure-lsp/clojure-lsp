(ns clojure-lsp.features.code-lens-test
  (:require
   [clojure-lsp.feature.code-lens :as f.code-lens]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(defn ^:private code-lens [components]
  (f.code-lens/reference-code-lens h/default-uri (h/db components)))

(defn ^:private code-lens-submap [[start-r start-c :as start] end]
  {:range (h/->range start end)
   :data [h/default-uri start-r start-c]})

(deftest reference-code-lens
  (testing "common lens"
    (let [components (h/make-components {:kondo-config {}})
          [def-start def-end
           defnp-start defnp-end
           defn-start defn-end]
          (h/load-code (str "(ns some-ns)\n"
                            "(def |foo| 1)\n"
                            "(defn- |foo2| []\n"
                            " foo)\n"
                            "(defn |bar| [a b]\n"
                            "  (+ a b (foo2)))\n"
                            "(s/defn baz []\n"
                            "  (bar 2 3))\n") h/default-uri components)]
      (h/assert-submaps
        [{}
         (code-lens-submap def-start def-end)
         (code-lens-submap defnp-start defnp-end)
         (code-lens-submap defn-start defn-end)]
        (code-lens components))))
  (testing "defrecord"
    (let [components (h/make-components {:kondo-config {}})
          [rec-start rec-end]
          (h/load-code (h/code "(defrecord |MyRecord| [])"
                               "(MyRecord)"
                               "(->MyRecord)"
                               "(map->MyRecord)") h/default-uri components)]
      (is (= [(code-lens-submap rec-start rec-end)]
             (code-lens components)))))
  (testing "keyword definitions"
    (let [components (h/make-components {:kondo-config {}})
          [event-start event-end
           sub-start sub-end]
          (h/load-code (h/code "(ns foo (:require [re-frame.core :as r]))"
                               "(r/reg-event-db |::event| identity)"
                               "(r/reg-sub |::sub| identity)") h/default-uri components)]
      (h/assert-submaps
        [{}
         (code-lens-submap event-start event-end)
         (code-lens-submap sub-start sub-end)]
        (code-lens components))))
  (testing "namespaces definitions"
    (let [components (h/make-components {:kondo-config {}})
          [ns-start ns-end]
          (h/load-code (h/code "(ns |bar| (:require [foo :as f])) f/a") h/default-uri components)]
      (h/assert-submaps
        [(code-lens-submap ns-start ns-end)]
        (code-lens components)))))

(defn ^:private code-lens-resolve [components {:keys [range data]}]
  (let [[uri row col] data]
    (f.code-lens/resolve-code-lens uri row col range (h/db components))))

(defn ^:private code-lens-resolve-submap [[start-r start-c :as start] end title]
  {:range (h/->range start end)
   :command {:title   title
             :command "code-lens-references"
             :arguments [h/default-uri start-r start-c]}})

(deftest test-code-lens-resolve
  (let [components (h/make-components)
        [ns-start ns-end
         def-start def-end
         defn-start defn-end]
        (h/load-code (str "(ns |some-ns|)\n"
                          "(def |foo| 1)\n"
                          "(defn- |foo2| []\n"
                          " foo)\n"
                          "(defn bar [a b]\n"
                          "  (+ a b (foo2)))\n"
                          "(s/defn baz []\n"
                          "  (bar 2 3))\n") h/default-uri components)
        [ns-code-lens
         def-code-lens
         defn-code-lens] (code-lens components)]
    (testing "references"
      (testing "empty lens"
        (is (= (code-lens-resolve-submap ns-start ns-end "0 references")
               (code-lens-resolve components ns-code-lens))))
      (testing "some lens"
        (is (= (code-lens-resolve-submap def-start def-end "1 reference")
               (code-lens-resolve components def-code-lens)))
        (is (= (code-lens-resolve-submap defn-start defn-end "1 reference")
               (code-lens-resolve components defn-code-lens))))
      (testing "defrecord lens"
        (let [components (h/make-components)
              [record-start record-end]
              (h/load-code (h/code "(defrecord |MyRecord| [])"
                                   "(MyRecord)"
                                   "(->MyRecord)"
                                   "(map->MyRecord)") h/default-uri components)
              [record-code-lens] (code-lens components)]
          (is (= (code-lens-resolve-submap record-start record-end "3 references")
                 (code-lens-resolve components record-code-lens))))))))
