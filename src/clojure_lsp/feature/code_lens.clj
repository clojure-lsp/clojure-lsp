(ns clojure-lsp.feature.code-lens
  (:require
   [clojure-lsp.db :as db]
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

(defn ^:private references->string [references s]
  (let [count (count references)
        msg (str count s)]
    (if (= 1 count)
      msg
      (str msg "s"))))

(defn ^:private main-references->string [references]
  (references->string references " reference"))

(defn ^:private test-references->string [references]
  (references->string references " test"))

(defn ^:private var-definitions-lens [filename settings analysis]
  (let [excluded-vars (set/union #{'clojure.test/deftest}
                                 (get-in settings [:linters :unused-public-var :exclude-when-defined-by] #{}))]
    (->> (q/find-var-definitions analysis filename true)
         (remove #(contains? excluded-vars (:defined-by %))))))

(defn ^:private keyword-definitions-lens
  [filename settings analysis]
  (let [excluded-vars (get-in settings [:linters :unused-public-var :exclude-when-defined-by] #{})]
    (->> (q/find-keyword-definitions analysis filename)
         (remove #(contains? excluded-vars (:reg %))))))

(defn reference-code-lens [uri]
  (let [analysis (:analysis @db/db)
        settings (:settings @db/db)
        filename (shared/uri->filename uri)]
    (->> (concat (var-definitions-lens filename settings analysis)
                 (keyword-definitions-lens filename settings analysis))
         (map (fn [element]
                {:range (shared/->range element)
                 :data  [uri (:name-row element) (:name-col element)]})))))

(defn test-reference? [source-path {:keys [filename]}]
  (and source-path
       (not (string/starts-with? filename source-path))
       (string/includes? filename "_test.")))

(defn resolve-code-lens [uri row col range]
  (let [filename (shared/uri->filename uri)
        segregate-lens? (get-in @db/db [:settings :lens-segregate-test-references] true)
        references (q/find-references-from-cursor (:analysis @db/db) filename row col false)]
    (if segregate-lens?
      (let [source-path (->> (get-in @db/db [:settings :source-paths])
                             (filter #(string/starts-with? filename %))
                             first)
            main-references (filter (complement (partial test-reference? source-path)) references)
            test-references (filter (partial test-reference? source-path) references)]
        (if (seq test-references)
          {:range range
           :command {:title (str (main-references->string main-references)
                                 " | "
                                 (test-references->string test-references))
                     :command "code-lens-references"
                     :arguments [uri row col]}}
          {:range range
           :command {:title (main-references->string main-references)
                     :command "code-lens-references"
                     :arguments [uri row col]}}))
      {:range range
       :command {:title (main-references->string  references)
                 :command "code-lens-references"
                 :arguments [uri row col]}})))
