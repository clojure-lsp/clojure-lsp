(ns clojure-lsp.feature.development-info
  (:require
   [clojure-lsp.feature.format :as f.format]
   [clojure-lsp.feature.semantic-tokens :as f.semantic-tokens]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.pprint :as pprint]
   [rewrite-clj.zip :as z]))

(defn ^:private server-info [db]
  {:project-root-uri (:project-root-uri db)
   :server-version (shared/clojure-lsp-version)
   :clj-kondo-version (lsp.kondo/clj-kondo-version)
   :log-path (:log-path db)
   :project-settings (:project-settings db)
   :classpath-settings (:classpath-settings db)
   :client-settings (:client-settings db)
   :final-settings (settings/all db)
   :classpath (:classpath db)
   :cljfmt-raw (binding [*print-meta* true]
                 (pr-str (f.format/resolve-user-cljfmt-config db)))
   :analysis-summary (q/analysis-summary db)
   :port (or (:port db)
             "NREPL only available on :debug profile (`bb debug-cli`)")})

(defn server-info-log [{:keys [producer db*]}]
  (producer/show-message
    producer
    (with-out-str (pprint/pprint (server-info @db*)))
    :info
    nil))

(defn server-info-raw [{:keys [db*]}]
  (shared/preserve-kebab-case (server-info @db*)))

(defn ^:private cursor-info [uri row col db]
  (let [elements (q/find-all-elements-under-cursor db uri row col)
        node (some-> (parser/safe-zloc-of-file db uri)
                     (parser/to-pos row col)
                     z/node)]
    (shared/assoc-some
      {}
      :node (if-let [children (:children node)]
              (-> node (dissoc :children) (assoc :children-count (count children)))
              node)
      :elements (mapv (fn [e]
                        (shared/assoc-some
                          {:element e}
                          :definition (q/find-definition db e)
                          :semantic-tokens (f.semantic-tokens/element->token-type e)))
                      elements))))

(defn cursor-info-log [uri {:keys [producer db*]} row col]
  (let [info (cursor-info uri row col @db*)]
    (producer/show-message
      producer
      (with-out-str (pprint/pprint info))
      :info
      nil)))

(defn cursor-info-raw [uri {:keys [db*]} row col]
  (shared/preserve-kebab-case (cursor-info uri row col @db*)))
