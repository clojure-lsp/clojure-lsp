(ns clojure-lsp.feature.diagnostics
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.kondo :as lsp.kondo]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [taoensso.timbre :as log]))

(def unnecessary-diagnostic-types
  #{:redefined-var
    :redundant-do
    :redundant-expression
    :redundant-let
    :unused-binding
    :unreachable-code
    :unused-import
    :unused-namespace
    :unused-private-var
    :unused-referred-var})

(def deprecated-diagnostic-types
  #{:deprecated-var})

(defn ^:private kondo-finding->diagnostic
  [{:keys [type message level row col end-row] :as finding}]
  (let [expression? (not= row end-row)
        finding (cond-> (merge {:end-row row :end-col col} finding)
                  expression? (assoc :end-row row :end-col col))]
    {:range (shared/->range finding)
     :tags (cond-> []
             (unnecessary-diagnostic-types type) (conj 1)
             (deprecated-diagnostic-types type) (conj 2))
     :message message
     :code (name type)
     :severity (case level
                 :error   1
                 :warning 2
                 :info    3)
     :source "clj-kondo"}))

(defn ^:private valid-finding? [{:keys [row col] :as finding}]
  (or (and row col)
      (log/warn "Invalid clj-kondo finding. Cannot find position data for" finding)))

(defn ^:private kondo-findings->diagnostics [filename findings]
  (->> (get findings filename)
       (filter #(= filename (:filename %)))
       (filter valid-finding?)
       (mapv kondo-finding->diagnostic)))

(defn ^:private find-diagnostics [uri db]
  (let [settings (get db :settings)
        filename (shared/uri->filename uri)]
    (cond-> []
      (not (= :off (get-in settings [:linters :clj-kondo :level])))
      (concat (kondo-findings->diagnostics filename (:findings db))))))

(defn sync-lint-file [uri db]
  (async/put! db/diagnostics-chan
              {:uri uri
               :diagnostics (find-diagnostics uri db)}))

(defn async-lint-file [uri db]
  (async/go
    (async/put! db/diagnostics-chan
                {:uri uri
                 :diagnostics (find-diagnostics uri db)})))

(defn ^:private reg-unused-public-var-elements! [elements reg-finding!]
  (doseq [element elements]
    (reg-finding! {:filename (:filename element)
                   :row (:name-row element)
                   :col (:name-col element)
                   :end-row (:name-end-row element)
                   :end-col (:name-end-col element)
                   :message (format "Unused public var '%s/%s'" (:ns element) (:name element))
                   :type :clojure-lsp/unused-public-var})))

(defn unused-public-var-lint-for-paths!
  [{:keys [analysis reg-finding!]}]
  (async/go
    (let [start-time (System/nanoTime)
          project-analysis (->> (lsp.kondo/normalize-analysis analysis)
                                (group-by :filename)
                                q/filter-project-analysis)
          elements (->> project-analysis
                        q/find-all-var-definitions
                        (filter (comp #(= (count %) 0)
                                      #(q/find-references project-analysis % false))))]
      (reg-unused-public-var-elements! elements reg-finding!)
      (log/info (format "Linting unused public vars for whole project took %sms" (quot (- (System/nanoTime) start-time) 1000000))))))

(defn unused-public-var-lint-for-single-file!
  [{:keys [analysis reg-finding!]}]
  (let [filename (-> analysis :var-definitions first :filename)
        project-analysis (->> (lsp.kondo/normalize-analysis analysis)
                              (assoc (:analysis @db/db) filename)
                              q/filter-project-analysis)
        elements (->> (q/find-var-definitions project-analysis filename false)
                      (filter (comp #(= (count %) 0)
                                    #(q/find-references project-analysis % false))))]
    (reg-unused-public-var-elements! elements reg-finding!)))
