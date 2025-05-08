(ns foo.my-bar
  (:require
   [clojure-lsp.custom-linters-api :as api])
  (:import
   [java.io File]))

(defn baz [{:keys [params db reg-diagnostic!]}]
  (reg-diagnostic! {:uri (str (:project-root-uri db) "src" File/separator "sample_test" File/separator "diagnostics" File/separator "custom_linters.clj")
                    :severity (:severity params)
                    :message (str "external analysis count: " (->> db
                                                                   api/internal-analysis
                                                                   vals
                                                                   (mapv :namespace-definitions)
                                                                   flatten
                                                                   (filter :external?)
                                                                   count))
                    :source "some-source"
                    :code "some-code"
                    :range {:start {:line 1 :character 2} :end {:line 3 :character 4}}}))
