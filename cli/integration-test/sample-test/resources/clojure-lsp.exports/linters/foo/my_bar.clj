(ns foo.my-bar
  (:require
   [clojure-lsp.custom-linters-api :as api]
   [clojure.java.io :as io]))

(defn baz [{:keys [params db reg-diagnostic!]}]
  (reg-diagnostic! {:uri (api/filename->uri
                           (.getCanonicalPath
                             (io/file (api/uri->filename (:project-root-uri db)) "src" "sample_test" "diagnostics" "custom_linters.clj"))
                           db)
                    :level (:level params)
                    :message (str "external analysis count: " (->> db
                                                                   api/internal-analysis
                                                                   vals
                                                                   (mapv :namespace-definitions)
                                                                   flatten
                                                                   (filter :external?)
                                                                   count))
                    :source "some-source"
                    :code "some-code"
                    :range {:row 1 :col 2 :end-row 3 :end-col 4}}))
