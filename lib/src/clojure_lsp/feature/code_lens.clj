(ns clojure-lsp.feature.code-lens
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

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

(defn reference-code-lens [uri db]
  (into []
        (map (fn [element]
               {:range (shared/->range element)
                :data  [uri (:name-row element) (:name-col element)]}))
        (concat (q/find-namespace-definitions db uri)
                (->> (concat (q/find-var-definitions db uri true)
                             (q/find-keyword-definitions db uri))
                     (remove (partial q/exclude-public-definition? (:kondo-config db)))))))

(defn resolve-code-lens [uri row col range db]
  (let [segregate-lens? (settings/get db [:code-lens :segregate-test-references] true)
        references (q/find-references-from-cursor db uri row col false)]
    (if segregate-lens?
      (let [source-uri (some-> uri
                               (shared/uri->source-path (settings/get db [:source-paths]))
                               (shared/filename->uri db))
            main-references (remove (comp (partial shared/test-reference? source-uri) :uri) references)
            test-references (filter (comp (partial shared/test-reference? source-uri) :uri) references)]
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
