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

(defn ^:private var-definitions-lens [db filename]
  (->> (q/find-var-definitions db filename true)
       (remove (partial q/exclude-public-definition? (:kondo-config db)))))

(defn ^:private keyword-definitions-lens
  [db filename]
  (->> (q/find-keyword-definitions db filename)
       (remove (partial q/exclude-public-definition? (:kondo-config db)))))

(defn reference-code-lens [uri db]
  (let [filename (shared/uri->filename uri)]
    (into []
          (map (fn [element]
                 {:range (shared/->range element)
                  :data  [uri (:name-row element) (:name-col element)]}))
          (concat (q/find-namespace-definitions db filename)
                  (var-definitions-lens db filename)
                  (keyword-definitions-lens db filename)))))

(defn test-reference? [source-path {:keys [filename]}]
  (and source-path
       (not (string/starts-with? filename source-path))
       (string/includes? filename "_test.")))

(defn resolve-code-lens [uri row col range db]
  (let [filename (shared/uri->filename uri)
        segregate-lens? (settings/get db [:code-lens :segregate-test-references] true)
        references (q/find-references-from-cursor db filename row col false)]
    (if segregate-lens?
      (let [source-path (->> (settings/get db [:source-paths])
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
