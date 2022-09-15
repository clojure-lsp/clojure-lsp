(ns clojure-lsp.features.file-management-test
  (:require
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.core.async :as async]
   [clojure.test :refer [are deftest is testing]]
   [medley.core :as medley]))

(h/reset-components-before-test)

(deftest update-text
  (is (= "(comment\n   )" (#'f.file-management/replace-text "(comment)" "\n   " 0 8 0 8)))
  (is (= "some \nboring\n text" (#'f.file-management/replace-text "some \ncool\n text" "boring" 1 0 1 4)))
  (is (= "(+ 1 2)" (#'f.file-management/replace-text "(+ 1 1)" "2" 0 5 0 6)))
  (is (= "(+ 1)" (#'f.file-management/replace-text "(+ 1 1)" "" 0 4 0 6)))
  (is (= "\n\n (+ 1 2)\n" (#'f.file-management/replace-text "\n\n (let [a 1\n   b 2]\n   (+ 1 2))\n" "(+ 1 2)" 2 1 4 11)))
  (is (= "\r\n\r\n (+ 1 2)\r\n" (#'f.file-management/replace-text "\r\n\r\n (let [a 1\r\n   b 2]\r\n   (+ 1 2))\r\n" "(+ 1 2)" 2 1 4 11)))
  (is (= "\n\n (let [a 1\n   b 2]\n   (+ 1 2))\n" (#'f.file-management/replace-text "\n\n (+ 1 2)\n" "(let [a 1\n   b 2]\n   (+ 1 2))" 2 1 2 8)))
  (is (= "(+ 1 1)\n\n" (#'f.file-management/replace-text "(+ 1 1)\n" "\n" 1 0 1 0))))

(deftest did-close
  (swap! (h/db*) medley/deep-merge {:settings {:source-paths #{(h/file-path "/user/project/src/clj")}}
                                    :project-root-uri (h/file-uri "file:///user/project")})
  (h/load-code-and-locs "(ns foo) a b c" (h/file-uri "file:///user/project/src/clj/foo.clj"))
  (h/load-code-and-locs "(ns bar) d e f" (h/file-uri "file:///user/project/src/clj/bar.clj"))
  (h/load-code-and-locs "(ns some-jar)" (h/file-uri "file:///some/path/to/jar.jar:/some/file.clj"))
  (testing "when file exists on disk"
    (let [mock-diagnostics-chan (async/chan 1)]
      (with-redefs [shared/file-exists? (constantly true)]
        (f.file-management/did-close "file:///user/project/src/clj/foo.clj" (assoc (h/components)
                                                                                   :diagnostics-chan mock-diagnostics-chan)))
      (is (get-in (h/db) [:analysis (h/file-path "/user/project/src/clj/foo.clj")]))
      (is (get-in (h/db) [:findings (h/file-path "/user/project/src/clj/foo.clj")]))
      (is (get-in (h/db) [:file-meta (h/file-path "/user/project/src/clj/foo.clj")]))
      (is (seq (get-in (h/db) [:dep-graph 'foo :uris])))
      (is (get-in (h/db) [:documents (h/file-uri "file:///user/project/src/clj/foo.clj")]))
      (h/assert-no-take mock-diagnostics-chan 500)))
  (testing "when local file not exists on disk"
    (let [mock-diagnostics-chan (async/chan 1)]
      (with-redefs [shared/file-exists? (constantly false)]
        (f.file-management/did-close (h/file-uri "file:///user/project/src/clj/bar.clj") (assoc (h/components)
                                                                                                :diagnostics-chan mock-diagnostics-chan)))
      (is (nil? (get-in (h/db) [:analysis (h/file-path "/user/project/src/clj/bar.clj")])))
      (is (nil? (get-in (h/db) [:findings (h/file-path "/user/project/src/clj/bar.clj")])))
      (is (nil? (get-in (h/db) [:file-meta (h/file-path "/user/project/src/clj/bar.clj")])))
      (is (not (seq (get-in (h/db) [:dep-graph 'bar :uris]))))
      (is (nil? (get-in (h/db) [:documents (h/file-uri "file:///user/project/src/clj/bar.clj")])))
      (is (= {:uri (h/file-uri "file:///user/project/src/clj/bar.clj")
              :diagnostics []}
             (h/take-or-timeout mock-diagnostics-chan 500)))))
  (testing "when file is external we do not remove analysis but publish empty diagnostics"
    (let [mock-diagnostics-chan (async/chan 1)]
      (with-redefs [shared/file-exists? (constantly false)]
        (f.file-management/did-close (h/file-uri "file:///some/path/to/jar.jar:/some/file.clj") (assoc (h/components)
                                                                                                       :diagnostics-chan mock-diagnostics-chan)))
      (is (get-in (h/db) [:analysis (h/file-path "/some/path/to/jar.jar:/some/file.clj")]))
      (is (get-in (h/db) [:findings (h/file-path "/some/path/to/jar.jar:/some/file.clj")]))
      (is (get-in (h/db) [:file-meta (h/file-path "/some/path/to/jar.jar:/some/file.clj")]))
      (is (seq (get-in (h/db) [:dep-graph 'some-jar :uris])))
      (is (get-in (h/db) [:documents (h/file-uri "file:///some/path/to/jar.jar:/some/file.clj")]))
      (is (= {:uri (h/file-uri "file:///some/path/to/jar.jar:/some/file.clj")
              :diagnostics []}
             (h/take-or-timeout mock-diagnostics-chan 500))))))

(deftest did-open
  (testing "on an empty file"
    (let [mock-edits-chan (async/chan 1)
          mock-diagnostics-chan (async/chan 1)
          filename (h/file-path "/user/project/src/aaa/bbb.clj")
          uri (h/file-uri "file:///user/project/src/aaa/bbb.clj")]
      (swap! (h/db*) shared/deep-merge {:settings {:auto-add-ns-to-new-files? true
                                                   :source-paths #{(h/file-path "/user/project/src")}}
                                        :project-root-uri (h/file-uri "file:///user/project")})
      (h/load-code-and-locs "" uri (assoc (h/components)
                                          :edits-chan mock-edits-chan
                                          :diagnostics-chan mock-diagnostics-chan))
      (is (get-in (h/db) [:analysis filename]))
      (is (get-in (h/db) [:findings filename]))
      (is (get-in (h/db) [:file-meta filename]))
        ;; The ns won't be in the dep graph until after the edit adding it is applied.
      (is (not (contains? (get (h/db) :dep-graph) 'aaa.bbb)))
      (is (get-in (h/db) [:documents uri]))
      (testing "should publish empty diagnostics"
        (is (= {:uri uri, :diagnostics []}
               (h/take-or-timeout mock-diagnostics-chan 500))))
      (testing "should add ns"
        (is (= {:changes
                {uri
                 [{:range
                   {:start {:line 0, :character 0},
                    :end {:line 999998, :character 999998}},
                   :new-text "(ns aaa.bbb)"}]}}
               (h/take-or-timeout mock-edits-chan 500)))))))

(deftest did-change
  (let [mock-changes-chan (async/chan 1)
        original-text (h/code "(ns aaa)"
                              "(def foo 1)")
        edited-text (h/code "(ns aaa)"
                            "(def bar 1)")]
    (h/load-code-and-locs original-text)
    (f.file-management/did-change h/default-uri
                                  [{:text "bar"
                                    :range {:start {:line 1 :character 5}
                                            :end {:line 1 :character 8}}}]
                                  2
                                  (assoc (h/components)
                                         :current-changes-chan mock-changes-chan))
    (is (= 2 (get-in (h/db) [:documents h/default-uri :v])))
    (is (= edited-text (get-in (h/db) [:documents h/default-uri :text])))
    (is (= {:uri h/default-uri, :text edited-text, :version 2}
           (h/take-or-timeout mock-changes-chan 500)))))

(deftest did-change-watched-files
  (testing "created file"
    (let [mock-watched-files-chan (async/chan 1)]
      (f.file-management/did-change-watched-files
        [{:type :created
          :uri h/default-uri}]
        (assoc (h/components)
               :watched-files-chan mock-watched-files-chan))
      (is (= h/default-uri (h/take-or-timeout mock-watched-files-chan 1000)))))
  (testing "changed file"
    (let [mock-watched-files-chan (async/chan 1)]
      (f.file-management/did-change-watched-files
        [{:type :changed
          :uri h/default-uri}]
        (assoc (h/components)
               :watched-files-chan mock-watched-files-chan))
      (is (= h/default-uri (h/take-or-timeout mock-watched-files-chan 1000)))))
  (testing "deleted file"
    (let [mock-diagnostics-chan (async/chan 1)]
      (f.file-management/did-change-watched-files
        [{:type :deleted
          :uri h/default-uri}]
        (assoc (h/components)
               :diagnostics-chan mock-diagnostics-chan))
      (is (= {:uri h/default-uri, :diagnostics []}
             (h/take-or-timeout mock-diagnostics-chan 500))))))

(deftest var-dependency-reference-filenames
  (swap! (h/db*) medley/deep-merge {:settings {:source-paths #{(h/file-path "/src")}}
                                    :project-root-uri (h/file-uri "file:///")})
  (h/load-code-and-locs (h/code "(ns a)"
                                "(def a)"
                                "(def b)") (h/file-uri "file:///src/a.clj"))
  (h/load-code-and-locs (h/code "(ns b (:require [a]))"
                                "(def x)"
                                "a/a"
                                "a/a") (h/file-uri "file:///src/b.clj"))
  (let [db-before (h/db)]
    (are [expected new-code]
         (do
           (h/load-code-and-locs new-code (h/file-uri "file:///src/b.clj"))
           (let [db-after (h/db)]
             (is (= expected
                    (f.file-management/reference-filenames (h/file-path (h/file-path "/src/b.clj")) db-before db-after)))))
      ;; increasing
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a"
                  "a/a"
                  "a/a")
      ;; decreasing
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a")
      ;; removing
      #{(h/file-path "/src/a.clj")} (h/code "(ns b (:require [a]))"
                                            "(def x)")
      ;; adding
      #{(h/file-path "/src/a.clj")} (h/code "(ns b (:require [a]))"
                                            "(def x)"
                                            "a/a"
                                            "a/a"
                                            "a/b")
      ;; same
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a"
                  "a/a")
      ;; external ns
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a"
                  "a/a"
                  "inc")
      ;; same ns
      #{} (h/code "(ns b (:require [a]))"
                  "(def x)"
                  "a/a"
                  "a/a"
                  "x"))))

(deftest kw-dependency-reference-filenames
  (swap! (h/db*) medley/deep-merge {:settings {:source-paths #{(h/file-path "/src")}}
                                    :project-root-uri (h/file-uri "file:///")})
  (h/load-code-and-locs (h/code "(ns aaa (:require [re-frame.core :as r]))"
                                "(r/reg-event-db :aaa/command identity)"
                                "(r/reg-event-db ::event identity)")
                        (h/file-uri "file:///src/aaa.clj"))
  (h/load-code-and-locs (h/code "(ns bbb (:require [re-frame.core :as r]))"
                                "(r/reg-event-db :bbb/command identity)"
                                ":aaa/command"
                                ":aaa/command")
                        (h/file-uri "file:///src/bbb.clj"))
  (let [db-before (h/db)]
    (are [expected new-code]
         (do
           (h/load-code-and-locs new-code (h/file-uri "file:///src/bbb.clj"))
           (let [db-after (h/db)]
             (is (= expected
                    (f.file-management/reference-filenames (h/file-path "/src/bbb.clj") db-before db-after)))))
      ;; increasing
      #{} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                  "(r/reg-event-db :bbb/command identity)"
                  ":aaa/command"
                  ":aaa/command"
                  ":aaa/command")
      ;; decreasing
      #{} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                  "(r/reg-event-db :bbb/command identity)"
                  ":aaa/command")
      ;; removing
      #{(h/file-path "/src/aaa.clj")} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                                              "(r/reg-event-db :bbb/command identity)")
      ;; adding
      #{(h/file-path "/src/aaa.clj")} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                                              "(r/reg-event-db :bbb/command identity)"
                                              ":aaa/command"
                                              ":aaa/command"
                                              ":aaa/event")
      ;; same
      #{} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                  "(r/reg-event-db :bbb/command identity)"
                  ":aaa/command"
                  ":aaa/command")
      ;; unregistered kw
      #{} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                  "(r/reg-event-db :bbb/command identity)"
                  ":aaa/command"
                  ":aaa/command"
                  ":unregistered")
      ;; same ns
      #{} (h/code "(ns bbb (:require [re-frame.core :as r]))"
                  "(r/reg-event-db :bbb/command identity)"
                  ":aaa/command"
                  ":aaa/command"
                  ":bbb/command"))))

(deftest var-dependent-reference-filenames
  (swap! (h/db*) medley/deep-merge {:settings {:source-paths #{(h/file-path "/src")}}
                                    :project-root-uri (h/file-uri "file:///")})
  (h/load-code-and-locs (h/code "(ns a)"
                                "(def a)"
                                "(def b)") (h/file-uri "file:///src/a.clj"))
  (h/load-code-and-locs (h/code "(ns b (:require [a]))"
                                "a/a"
                                "a/c") (h/file-uri "file:///src/b.clj"))
  (let [db-before (h/db)]
    (are [expected new-code]
         (do
           (h/load-code-and-locs new-code (h/file-uri "file:///src/a.clj"))
           (let [db-after (h/db)]
             (is (= expected
                    (f.file-management/reference-filenames (h/file-path "/src/a.clj") db-before db-after)))))
      ;; remove existing
      #{(h/file-path "/src/b.clj")} (h/code "(ns a)"
                                            "(def b)")
      ;; create missing
      #{(h/file-path "/src/b.clj")} (h/code "(ns a)"
                                            "(def a)"
                                            "(def b)"
                                            "(def c)")
      ;; remove unused
      #{} (h/code "(ns a)"
                  "(def a)")
      ;; add unused
      #{} (h/code "(ns a)"
                  "(def a)"
                  "(def b)"
                  "(def d)"))))

(deftest will-rename-files
  (testing "when namespace matches old file"
    (swap! (h/db*) shared/deep-merge {:settings {:source-paths #{(h/file-path "/user/project/src")}}
                                      :project-root-uri (h/file-uri "file:///user/project")
                                      :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})

    (let [old-uri (h/file-uri "file:///user/project/src/my/ns.clj")
          new-uri (h/file-uri "file:///user/project/src/my/new/ns.clj")]
      (h/load-code (h/code "(ns my.ns)") old-uri)
      (let [db (h/db)]
        (is (= {:document-changes
                [{:text-document
                  {:version 0, :uri (h/file-uri "file:///user/project/src/my/ns.clj")},
                  :edits
                  [{:range
                    {:start {:line 0, :character 4}, :end {:line 0, :character 9}},
                    :new-text "my.new.ns"}]}]}
               (f.file-management/will-rename-files
                 [{:old-uri old-uri
                   :new-uri new-uri}]
                 db))))))
  (testing "when namespace matches new file"
    ;; This happens when namespace was already changed by textDocument/rename
    (swap! (h/db*) shared/deep-merge {:settings {:source-paths #{(h/file-path "/user/project/src")}}
                                      :project-root-uri (h/file-uri "file:///user/project")
                                      :client-capabilities {:workspace {:workspace-edit {:document-changes true}}}})

    (let [old-uri (h/file-uri "file:///user/project/src/my/ns.clj")
          new-uri (h/file-uri "file:///user/project/src/my/new/ns.clj")]
      (h/load-code (h/code "(ns my.new.ns)") old-uri)
      (let [db (h/db)]
        (is (= {:document-changes []}
               (f.file-management/will-rename-files
                 [{:old-uri old-uri
                   :new-uri new-uri}]
                 db)))))))
