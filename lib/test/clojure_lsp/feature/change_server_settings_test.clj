(ns clojure-lsp.feature.change-server-settings-test
  (:require
   [clojure-lsp.feature.clean-ns :as f.clean-ns]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]
   [rewrite-clj.zip :as z]))

(def dummy-empty-file "")
(def project-uri (h/file-uri "file:///user/project"))
(def project-source-paths #{(h/file-uri "/user/project/src")})

(defn- run-clean-ns-and-check-result
  ([input-code expected-code]
   (run-clean-ns-and-check-result input-code expected-code true))
  ([input-code expected-code in-form]
   (run-clean-ns-and-check-result input-code expected-code in-form "file:///a.clj"))
  ([input-code expected-code in-form uri]
   (h/load-code-and-locs input-code (h/file-uri uri))
   (let [zloc (when in-form
                (-> (z/of-string input-code) z/down z/right z/right))
         [{:keys [loc range]}] (f.clean-ns/clean-ns-edits zloc (h/file-uri uri) (h/db))]
     (is (some? range))
     (is (= expected-code
            (z/root-string loc))))))

(settings/set-all (h/db*)
                  {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                   :auto-add-ns-to-new-files? true})

(deftest change-settings
  (testing "Changing [:settings :clean :sort :require] setting while server is running"
    (h/reset-components!)

    ;; Set settings that don't sort the requires
    (settings/set-all (h/db*)
                      {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                       :clean {:sort {:require false}}})

    ;; Run run-clean-ns and verify that the requires isn't changed
    (run-clean-ns-and-check-result
      (h/code "(ns project.a"
              "  (:require"
              "   [a.a :as a]"
              "   [c.c :as c]"
              "   [b.b :as b]))"
              "(a/foo 1)"
              "(b/bar 2)"
              "(c/something 123)")
      (h/code "(ns project.a"
              "  (:require"
              "   [a.a :as a]"
              "   [c.c :as c]"
              "   [b.b :as b]))"
              "(a/foo 1)"
              "(b/bar 2)"
              "(c/something 123)")
      true
      "file:///project/a.clj")

    ;; Change settings to sort requires
    (settings/set-all (h/db*) {:clean {:sort {:require true}}})

    ;; Run run-clean-ns and verify that the requires was sorted
    (run-clean-ns-and-check-result
      (h/code "(ns project.a"
              "  (:require"
              "   [a.a :as a]"
              "   [c.c :as c]"
              "   [b.b :as b]))"
              "(a/foo 1)"
              "(b/bar 2)"
              "(c/something 123)")
      (h/code "(ns project.a"
              "  (:require"
              "   [a.a :as a]"
              "   [b.b :as b]"
              "   [c.c :as c]))"
              "(a/foo 1)"
              "(b/bar 2)"
              "(c/something 123)")
      true
      "file:///project/a.clj"))

  (testing "Changing [:settings :auto-add-ns-to-new-files?] setting while server is running"
    ;; Reset the (h/db)
    (h/reset-components!)

    ;; Loads a project on (h/db)
    (h/load-project project-uri project-source-paths)

    ;; Set settings that don't add ns to new files
    (settings/set-all (h/db*)
                      {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                       :auto-add-ns-to-new-files? false})

    (let [create-ns-changes (f.file-management/create-ns-changes (h/file-uri (str project-uri "/src/models/my_model.clj"))
                                                                 dummy-empty-file
                                                                 (h/db))]
      ;;Verify if create-ns-changes returns nil because settings are set to not add ns to new files
      (is (nil? create-ns-changes)))

    ;; Set settings that do add ns to new files
    (settings/set-all (h/db*)
                      {:client-capabilities {:workspace {:workspace-edit {:document-changes true}}}
                       :auto-add-ns-to-new-files? true})

    (let [create-ns-changes (f.file-management/create-ns-changes (h/file-uri (str project-uri "/src/models/my_model.clj"))
                                                                 dummy-empty-file
                                                                 (h/db))]
      ;;Verify if create-ns-changes returns nil because settings are set to not add ns to new files
      (is (not (nil? create-ns-changes)))

      (is (= "(ns models.my-model)"
             (-> create-ns-changes
                 :changes
                 (get (str project-uri "/src/models/my_model.clj"))
                 first
                 :new-text))))))
