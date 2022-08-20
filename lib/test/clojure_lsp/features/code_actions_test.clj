(ns clojure-lsp.features.code-actions-test
  (:require
   [clojure-lsp.feature.code-actions :as f.code-actions]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.test-helper :as h]
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]))

(defn code-actions
  [components uri [row col :as row-col] diagnostics]
  (let [{:keys [db client-capabilities]} (h/snapc components)
        position (h/->position row-col)]
    (f.code-actions/all (parser/safe-zloc-of-file db uri)
                        uri
                        row
                        col
                        (map (fn [d]
                               (update-in d [:range :start] #(or % position)))
                             diagnostics)
                        client-capabilities
                        db)))

(deftest add-alias-suggestion-code-actions
  (let [components (h/make-components)]
    (h/load-code "(ns clojure.set)" (h/file-uri "file:///clojure.core.clj") components)
    (h/load-code "(ns medley.core)" (h/file-uri "file:///medley.core.clj") components)
    (h/load-code "(ns clojure.data.json)" (h/file-uri "file:///clojure.data.json.clj") components)
    (h/load-code "(ns some (:require [chesire :as json]))" (h/file-uri "file:///some.clj") components)
    (let [[set-rc medley-rc json-rc]
          (h/load-code (h/code "(ns some)"
                               "(cl|ojure.set/union #{} #{})"
                               "(me|dley.core/foo 1 2)"
                               "(cl|ojure.data.json/bar 1 2)") h/default-uri components)
          code-actions #(code-actions components h/default-uri % [{:code  "unresolved-namespace"}])]
      (testing "simple ns"
        (h/assert-contains-submaps
          [{:title "Add require '[clojure.set :as set]'"
            :command {:command "add-require-suggestion"}}]
          (code-actions set-rc)))
      (testing "core ns"
        (h/assert-contains-submaps
          [{:title "Add require '[medley.core :as medley]'"
            :command {:command "add-require-suggestion"}}]
          (code-actions medley-rc)))
      (testing "already used alias, we add proper suggestion"
        (h/assert-contains-submaps
          [{:title "Add require '[clojure.data.json :as data.json]'"
            :command {:command "add-require-suggestion"}}]
          (code-actions json-rc))))))

(deftest add-refer-suggestion-code-actions
  (let [components (h/make-components)]
    (h/load-code "(ns clojure.set) (defn union [])" (h/file-uri "file:///clojure.core.clj") components)
    (h/load-code "(ns medley.core) (def unit) (defn uni [])" (h/file-uri "file:///medley.core.clj") components)
    (h/load-code "(ns some (:require [clojure.set :refer [union]]))" (h/file-uri "file:///some.clj") components)
    (let [[unit-rc union-rc]
          (h/load-code (h/code "(ns a)"
                               "(u|nit 1)"
                               "(u|nion #{} #{})") h/default-uri components)
          code-actions #(code-actions components h/default-uri %1 %2)]
      (testing "single suggestion"
        (h/assert-contains-submaps
          [{:title "Add require '[medley.core :refer [unit]]'"
            :command {:command "add-require-suggestion"}}]
          (code-actions unit-rc [{:code    "unresolved-symbol"
                                  :message "Unresolved symbol: unit"}])))
      (testing "multiple suggestions"
        (h/assert-contains-submaps
          [{:title   "Add require '[clojure.set :refer [union]]'"
            :command {:command "add-require-suggestion"}}]
          (code-actions union-rc [{:code    "unresolved-symbol"
                                   :message "Unresolved symbol: union"}]))))))

(deftest add-missing-namespace-code-actions
  (let [components (h/make-components)]
    (h/load-code (str "(ns some-ns)\n"
                      "(def foo)")
                 (h/file-uri "file:///a.clj") components)
    (h/load-code (str "(ns other-ns (:require [some-ns :as sns]))\n"
                      "(def bar 1)\n"
                      "(defn baz []\n"
                      "  bar)")
                 (h/file-uri "file:///b.clj") components)
    (let [[cursor-1 cursor-2 cursor-3 cursor-4 cursor-5]
          (h/load-code (str "(ns another-ns)\n"
                            "(def bar |o|ns/bar)\n"
                            "(def foo s|ns/foo)\n"
                            "(|deftest s|ome-test)\n"
                            "MyClass.\n"
                            "Date.\n"
                            "Date/parse")
                       (h/file-uri "file:///c.clj") components)
          code-actions #(code-actions components "file:///c.clj" %1 %2)]
      (testing "when it has not unresolved-namespace diagnostic"
        (is (not-any? #(string/starts-with? (:title %) "Add require")
                      (code-actions cursor-1 []))))
      (testing "when it has unresolved-namespace but cannot find namespace"
        (is (not-any? #(string/starts-with? (:title %) "Add require")
                      (code-actions cursor-2 [{:code  "unresolved-namespace"}]))))
      (testing "when it has unresolved-namespace and can find namespace"
        (h/assert-contains-submaps
          [{:title "Add require '[some-ns :as sns]' × 1"
            :command {:command "add-require-suggestion"}}]
          (code-actions cursor-3 [{:code "unresolved-namespace"}])))
      (testing "when it has unresolved-symbol and it's a known refer"
        (h/assert-contains-submaps
          [{:title "Add require '[clojure.test :refer [deftest]]'"
            :command {:command "add-require-suggestion"}}]
          (code-actions cursor-4 [{:code "unresolved-namespace"}])))
      (testing "when it has unresolved-symbol but it's not a known refer"
        (is (not-any? #(string/starts-with? (:title %) "Add require")
                      (code-actions cursor-5 [{:code "unresolved-symbol"
                                               :message "Unresolved symbol: foo"}])))))))

(deftest add-common-missing-import-code-action
  (let [components (h/make-components)
        [custom-class-rc common-class-rc common-class-fn-rc]
        (h/load-code (str "(ns another-ns)\n"
                          "(def bar ons/bar)\n"
                          "(def foo sns/foo)\n"
                          "(deftest some-test)\n"
                          "M|yClass.\n"
                          "D|ate.\n"
                          "D|ate/parse")
                     (h/file-uri "file:///c.clj") components)
        code-actions #(code-actions components "file:///c.clj" %1 %2)]
    (testing "when it has no unresolved-symbol diagnostic"
      (code-actions custom-class-rc []))

    (testing "when it has unresolved-symbol but it's not a common import"
      (is (not-any? #(string/starts-with? (:title %) "Add import")
                    (code-actions custom-class-rc [{:code "unresolved-symbol"
                                                    :message "Unresolved symbol: foo"}]))))

    (testing "when it has unresolved-symbol and it's a common import"
      (h/assert-contains-submaps
        [{:title "Add import 'java.util.Date'"
          :command {:command "add-missing-import"}}]
        (code-actions common-class-rc [{:code "unresolved-symbol"
                                        :message "Unresolved symbol: foo"}])))

    (testing "when it has unresolved-namespace and it's a common import via method"
      (h/assert-contains-submaps
        [{:title "Add import 'java.util.Date'"
          :command {:command "add-missing-import"}}]
        (code-actions common-class-fn-rc [{:code "unresolved-namespace"}])))))

(deftest inline-symbol-code-action
  (let [components (h/make-components)
        [defn-rc var-usage-rc]
        (h/load-code (str "(ns other-ns (:require [some-ns :as sns]))\n"
                          "(def bar 1)\n"
                          "(|defn baz []\n"
                          "  ba|r)")
                     (h/file-uri "file:///b.clj") components)
        code-actions #(code-actions components "file:///b.clj" %1 [])]
    (testing "when in not a let/def symbol"
      (is (not-any? #(= (:title %) "Inline symbol")
                    (code-actions defn-rc))))
    (testing "when in let/def symbol"
      (h/assert-contains-submaps
        [{:title "Inline symbol"
          :command {:command "inline-symbol"}}]
        (code-actions var-usage-rc)))))

(deftest change-coll-code-action
  (let [components (h/make-components)
        [str-rc list-rc map-rc vec-rc set-rc]
        (h/load-code (h/code "|\"some string\""
                             "|(some-function 1 2)"
                             "|{:some :map}"
                             "|[:some :vector]"
                             "|#{:some :set}")
                     (h/file-uri "file:///b.clj") components)
        code-actions #(code-actions components "file:///b.clj" %1 [])]
    (testing "when in not a coll"
      (is (not-any? #(= (:title %) "Change coll to")
                    (code-actions str-rc))))
    (testing "when in a list"
      (h/assert-contains-submaps
        [{:title "Change coll to map"
          :command {:command "change-coll"}}]
        (code-actions list-rc)))
    (testing "when in a map"
      (h/assert-contains-submaps
        [{:title   "Change coll to vector"
          :command {:command "change-coll"}}]
        (code-actions map-rc)))
    (testing "when in a vector"
      (h/assert-contains-submaps
        [{:title "Change coll to set"
          :command {:command "change-coll"}}]
        (code-actions vec-rc)))
    (testing "when in a set"
      (h/assert-contains-submaps
        [{:title "Change coll to list"
          :command {:command "change-coll"}}]
        (code-actions set-rc)))))

(deftest introduce-let-code-action
  (let [components (h/make-components)
        [form-rc] (h/load-code (h/code "(+ |(- 10 3) 2)") h/default-uri components)
        code-actions #(code-actions components h/default-uri %1 [])]
    (testing "when a valid zloc"
      (h/assert-contains-submaps
        [{:title "Introduce let"
          :command {:command "introduce-let"}}]
        (code-actions form-rc)))
    (testing "when not a valid zloc"
      (is (not-any? #(= (:title %) "Introduce let")
                    (code-actions [4 14]))))))

(deftest move-to-let-code-action
  (let [components (h/make-components)
        [form-rc]
        (h/load-code (h/code "(let [a 1"
                             "      b 2]"
                             "  |(+ 1 2))"
                             "(+ 1 2)")
                     (h/file-uri "file:///b.clj") components)
        code-actions #(code-actions components "file:///b.clj" %1 [])]
    (testing "when not a valid zloc"
      (is (not-any? #(= (:title %) "Move to let")
                    (code-actions [4 14]))))
    (testing "when inside let form"
      (h/assert-contains-submaps
        [{:title "Move to let"
          :command {:command "move-to-let"}}]
        (code-actions form-rc)))))

(deftest cycle-privacy-code-action
  (let [components (h/make-components)
        [ns-rc def-rc]
        (h/load-code (str "(ns |some-ns)\n"
                          "(def| foo)")
                     (h/file-uri "file:///a.clj") components)
        code-actions #(code-actions components "file:///a.clj" %1 [])]
    (testing "when non function location"
      (is (not-any? #(= (:title %) "Cycle privacy")
                    (code-actions ns-rc))))
    (testing "when on function location"
      (h/assert-contains-submaps
        [{:title "Cycle privacy"
          :command {:command "cycle-privacy"}}]
        (code-actions def-rc)))))

(deftest destructure-keys-code-action
  (let [components (h/make-components)
        [non-local-rc local-rc]
        (h/load-code (h/code "(ns some-ns)"
                             "(def |foo)"
                             "(defn bar [|shape]"
                             "  (:shape/type shape))")
                     (h/file-uri "file:///a.clj") components)
        code-actions #(code-actions components "file:///a.clj" %1 [])]
    (testing "when not on local"
      (is (not-any? #(= (:title %) "Destructure keys")
                    (code-actions non-local-rc))))
    (testing "when on local"
      (h/assert-contains-submaps
        [{:title "Destructure keys"
          :command {:command "destructure-keys"}}]
        (code-actions local-rc)))))

(deftest restructure-keys-code-action
  (let [components (h/make-components)
        [non-restructurable restructurable]
        (h/load-code (h/code "(ns some-ns)"
                             "(def |foo)"
                             "(defn bar [|{:keys [shape/type]}]"
                             "  type)")
                     (h/file-uri "file:///a.clj") components)
        code-actions #(code-actions components "file:///a.clj" %1 [])]
    (testing "when not on restructurable map"
      (is (not-any? #(= (:title %) "Restructure keys")
                    (code-actions non-restructurable))))
    (testing "when on restructurable map"
      (h/assert-contains-submaps
        [{:title "Restructure keys"
          :command {:command "restructure-keys"}}]
        (code-actions restructurable)))))

(deftest extract-function-code-action
  (let [components (h/make-components)
        [ns-rc def-rc]
        (h/load-code (str "(ns |some-ns)\n"
                          "(def| foo)")
                     (h/file-uri "file:///a.clj") components)
        code-actions #(code-actions components "file:///a.clj" %1 [])]
    (testing "when non function location"
      (is (not-any? #(= (:title %) "Extract function")
                    (code-actions ns-rc))))
    (testing "when on function location"
      (h/assert-contains-submaps
        [{:title "Extract function"
          :command {:command "extract-function"}}]
        (code-actions def-rc)))))

(deftest extract-to-def-code-action
  (let [components (h/make-components)
        [map-rc]
        (h/load-code "|{:a 1}"
                     (h/file-uri "file:///a.clj") components)
        code-actions #(code-actions components "file:///a.clj" %1 [])]
    (h/assert-contains-submaps
      [{:title "Extract to def"
        :command {:command "extract-to-def"}}]
      (code-actions map-rc))))

(deftest create-private-function-code-action
  (let [components (h/make-components)
        [ns-rc form-rc]
        (h/load-code (h/code "(ns some-|ns)"
                             "(def foo |(+ 1 2))"
                             "(def bar (some-func 1 2))") h/default-uri components)
        code-actions #(code-actions components h/default-uri %1 %2)]
    (testing "when not in a unresolved symbol"
      (is (not-any? #(= (:title %) "Create private function")
                    (code-actions ns-rc []))))
    (testing "when in a unresolved symbol"
      (h/assert-contains-submaps
        [{:title "Create private function 'some-func'"
          :command {:command "create-function"}}]
        (code-actions form-rc [{:code    "unresolved-symbol"
                                :message "Unresolved symbol: some-func"}])))))

(deftest thread-get-actions
  (let [components (h/make-components)
        code-actions #(code-actions components h/default-uri %1 [])]
    (let [[rc] (h/load-code (h/code "|(:z (:y (:x m)))") h/default-uri components)]
      (h/assert-contains-submaps
        [{:title "Move another expression to get/get-in"
          :command {:command "get-in-more"}}
         {:title "Move all expressions to get/get-in"
          :command {:command "get-in-all"}}]
        (code-actions rc)))
    (let [[rc] (h/load-code (h/code "|(get-in m [:x :y :z])") h/default-uri components)]
      (h/assert-contains-submaps
        [{:title "Remove one element from get/get-in"
          :command {:command "get-in-less"}}
         {:title "Unwind whole get/get-in"
          :command {:command "get-in-none"}}]
        (code-actions rc)))))

(deftest thread-first-all-action
  (let [components (h/make-components)
        [ns-rc outside-def-rc inside-def-rc outside-form-rc inside-form-rc]
        (h/load-code (h/code "|(ns some-ns)"
                             "|(|def foo)"
                             "|(-| (+ 1 1) 2)")
                     (h/file-uri "file:///a.clj") components)
        code-actions #(code-actions components "file:///a.clj" %1 [])]
    (testing "when in a ns or :require"
      (is (not-any? #(= (:title %) "Thread first all")
                    (code-actions ns-rc))))
    (testing "when in a def similar location"
      (is (not-any? #(= (:title %) "Thread first all")
                    (code-actions outside-def-rc))))
    (testing "when on a def non-list node"
      (is (not-any? #(= (:title %) "Thread first all")
                    (code-actions inside-def-rc))))
    (testing "when on a valid function that can be threaded"
      (h/assert-contains-submaps
        [{:title "Thread first all"
          :command {:command "thread-first-all"}}]
        (code-actions outside-form-rc)))
    (testing "when on a non-list node"
      (h/assert-contains-submaps
        [{:title "Thread first all"
          :command {:command "thread-first-all"}}]
        (code-actions inside-form-rc)))))

(deftest thread-last-all-action
  (let [components (h/make-components)
        [ns-rc outside-def-rc inside-def-rc outside-form-rc inside-form-rc]
        (h/load-code (h/code "|(ns some-ns)"
                             "|(|def foo)"
                             "|(-| (+ 1 1) 2)")
                     (h/file-uri "file:///a.clj") components)
        code-actions #(code-actions components "file:///a.clj" %1 [])]
    (testing "when in a ns or :require"
      (is (not-any? #(= (:title %) "Thread last all")
                    (code-actions ns-rc))))
    (testing "when in a def similar location"
      (is (not-any? #(= (:title %) "Thread last all")
                    (code-actions outside-def-rc))))
    (testing "when on a def non-list node"
      (is (not-any? #(= (:title %) "Thread last all")
                    (code-actions inside-def-rc))))
    (testing "when on a valid function that can be threaded"
      (h/assert-contains-submaps
        [{:title "Thread last all"
          :command {:command "thread-last-all"}}]
        (code-actions outside-form-rc)))
    (testing "when on a non-list node"
      (h/assert-contains-submaps
        [{:title "Thread last all"
          :command {:command "thread-last-all"}}]
        (code-actions inside-form-rc)))))

(deftest unwind-thread-action
  (let [components (h/make-components)
        [ns-rc outside-thread-rc on-thread-rc inside-thread-rc]
        (h/load-code (h/code "|(ns some-ns)"
                             "(def foo)"
                             "|(|->> (+ 0 1)"
                             "     (+ 2)"
                             "     (|+ 3))")
                     (h/file-uri "file:///a.clj") components)
        code-actions #(code-actions components "file:///a.clj" %1 [])]
    (testing "when not in a thread"
      (is (not-any? #(= (:title %) "Unwind thread once")
                    (code-actions ns-rc))))
    (testing "when inside thread call"
      (h/assert-contains-submaps
        [{:title "Unwind thread once"
          :command {:command "unwind-thread"}}]
        (code-actions outside-thread-rc)))
    (testing "when inside thread symbol"
      (h/assert-contains-submaps
        [{:title "Unwind thread once"
          :command {:command "unwind-thread"}}]
        (code-actions on-thread-rc)))
    (testing "when inside any threading call"
      (h/assert-contains-submaps
        [{:title "Unwind thread once"
          :command {:command "unwind-thread"}}]
        (code-actions inside-thread-rc)))))

(deftest clean-ns-code-actions
  (let [components (h/make-components)]
    (h/load-code (str "(ns some-ns)\n"
                      "(def foo)")
                 (h/file-uri "file:///a.clj") components)
    (h/load-code (str "(ns another-ns)\n"
                      "(def bar ons/bar)\n"
                      "(def foo sns/foo)\n"
                      "(deftest some-test)\n"
                      "MyClass.\n"
                      "Date.\n"
                      "Date/parse")
                 (h/file-uri "file:///c.clj") components)
    (let [[def-rc]
          (h/load-code (str "(ns other-ns (:require [some-ns :as sns]))\n"
                            "(|def bar 1)\n"
                            "(defn baz []\n"
                            "  bar)")
                       (h/file-uri "file:///b.clj") components)
          code-actions #(code-actions components "file:///b.clj" def-rc [])]
      (testing "without workspace edit client capability"
        (is (not-any? #(= (:title %) "Clean namespace")
                      (code-actions))))
      (testing "with workspace edit client capability"
        (swap! (:db* components) assoc-in [:client-capabilities :workspace :workspace-edit] true)
        (h/assert-contains-submaps
          [{:title "Clean namespace"
            :command {:command "clean-ns"}}]
          (code-actions))))))

(deftest resolve-macro-as-code-actions
  (let [components (h/make-components)
        [macro-body fn-body]
        (h/load-code (h/code "(ns some-ns)"
                             "(defmacro foo [name & body] @body)"
                             "(foo m|y-fn)"
                             "(+ |1 2)") h/default-uri components)
        code-actions #(code-actions components h/default-uri %1 [])]
    (testing "when inside a macro usage"
      (h/assert-contains-submaps
        [{:title "Resolve macro 'some-ns/foo' as..."
          :command {:command "resolve-macro-as"}}]
        (code-actions macro-body)))
    (testing "when not inside a macro usage"
      (is (not-any? #(= (:title %) "Resolve macro 'some-ns/foo' as...")
                    (code-actions fn-body))))))

(deftest suppress-diagnostic-code-actions
  (let [components (h/make-components {:client-capabilities {:workspace {:workspace-edit true}}})
        [rc] (h/load-code (h/code "(ns some-ns)"
                                  ""
                                  "(de|f ^:private a 1)") h/default-uri components)]
    (testing "unused-private-var"
      (h/assert-contains-submaps
        [{:title "Suppress 'unused-private-var' diagnostic"
          :command {:command "suppress-diagnostic"}}]
        (code-actions components h/default-uri rc
                      [{:code    "unused-private-var"
                        :message "Unused private var: a"}])))))

(deftest sort-clauses-actions
  (let [components (h/make-components {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})
        [var-rc map-rc kw-rc]
        (h/load-code (h/code "(ns some-ns)"
                             ""
                             "(defn |foo []"
                             "  |{:|g 2 :s 3 :kj 3 :a 5})") h/default-uri components)
        code-actions #(code-actions components h/default-uri %1 [])]
    (testing "on map bracket"
      (h/assert-contains-submaps
        [{:title "Sort map"
          :command {:command "sort-clauses"}}]
        (code-actions map-rc)))
    (testing "On map's key"
      (h/assert-contains-submaps
        [{:title "Sort map"
          :command {:command "sort-clauses"}}]
        (code-actions kw-rc)))
    (testing "not on map"
      (is (not-any? #(= (:title %) "Sort map")
                    (code-actions var-rc))))))

(deftest create-test-code-actions
  (let [components (h/make-components {:settings {:source-paths #{(h/file-path "/project/src") (h/file-path "/project/test")}}
                                       :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                                       :project-root-uri (h/file-uri "file:///project")})
        [defn-rc]
        (h/load-code (h/code "(ns some-ns)"
                             ""
                             "(defn| foo [] 1)")
                     "file:///project/src/some_ns.clj" components)
        code-actions #(code-actions components "file:///project/src/some_ns.clj" %1 [])]
    (testing "inside function"
      (h/assert-contains-submaps
        [{:title "Create test for 'foo'"
          :command {:command "create-test"}}]
        (code-actions defn-rc)))))

(deftest promote-fn-actions
  (let [components (h/make-components)
        [fn-rc anon-rc]
        (h/load-code (h/code "|(fn [] (+ 1 2))"
                             "|#(+ 1 2)") h/default-uri components)
        code-actions #(code-actions components h/default-uri %1 [])]
    (h/assert-contains-submaps
      [{:title "Promote fn to defn"
        :command {:command "promote-fn"}}]
      (code-actions fn-rc))
    (h/assert-contains-submaps
      [{:title "Promote #() to fn"
        :command {:command "promote-fn"}}]
      (code-actions anon-rc))))

(deftest demote-fn-actions
  (let [components (h/make-components)
        [fn-rc] (h/load-code (h/code "|(fn [] (+ 1 2))") h/default-uri components)
        code-actions #(code-actions components h/default-uri %1 [])]
    (h/assert-contains-submaps
      [{:title "Demote fn to #()"
        :command {:command "demote-fn"}}]
      (code-actions fn-rc))))
