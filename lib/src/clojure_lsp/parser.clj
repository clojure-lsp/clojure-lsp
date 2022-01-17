(ns clojure-lsp.parser
  (:require
   [clojure-lsp.refactor.edit :as edit]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro zspy [loc]
  `(do
     (log/warn '~loc (pr-str (z/sexpr ~loc)))
     ~loc))

(defn ident-split [ident-str]
  (let [ident-conformed (some-> ident-str (string/replace #"^::?" ""))
        prefix (string/replace ident-str #"^(::?)?.*" "$1")
        idx (string/index-of ident-conformed "/")]
    (if (and idx (not= idx (dec (count ident-conformed))))
      (into [prefix] (string/split ident-conformed #"/" 2))
      [prefix nil ident-conformed])))

(defn same-range? [{:keys [name-row name-col name-end-row name-end-col] :as _a-pos}
                   {r :name-row c :name-col er :name-end-row ec :name-end-col :as _b-pos}]
  (and (= r name-row)
       (= er name-end-row)
       (= c name-col)
       (= ec name-end-col)))

(defn find-top-forms-in-range
  [code pos]
  (->> (edit/find-forms (z/of-string code) #(edit/in-range? pos (-> % z/node meta)))
       (mapv (fn [loc] (z/find loc z/up edit/top?)))
       (distinct)))

(defn ^:private handle-end-slash-code [text exception]
  (when-let [[_ token] (->> exception
                            Throwable->map
                            :cause
                            (re-matches #"Invalid symbol: (.*\/)."))]
    (let [token-pattern (re-pattern (str token "(\\s|\\n|\\))"))]
      (when-let [[_ extra-token] (re-find token-pattern text)]
        (-> text
            (string/replace-first token-pattern (str token "_" extra-token))
            z/of-string
            (z/edit->
              (z/find-next-value z/next (symbol (str token "_")))
              (z/replace (n/token-node (symbol token))))
            z/up)))))

(defn ^:private handle-single-colon-code [text exception]
  (let [cause (->> exception Throwable->map :cause)]
    (when (or (re-matches #"\[line (\d+), col (\d+)\] A single colon is not a valid keyword." cause)
              (re-matches #"\[line (\d+), col (\d+)\] Invalid keyword: ." cause))
      (let [colon-pattern (re-pattern ":(\\s|\\n|\\))")]
        (when-let [[_ extra-token] (re-find colon-pattern text)]
          (-> text
              (string/replace-first colon-pattern (str ":___" extra-token))
              z/of-string
              (z/edit->
                (z/find-next-value z/next :___)
                (z/replace (n/token-node (symbol ":"))))
              z/up))))))

(defn ^:private handle-keyword-with-end-slash-code [text exception]
  (when-let [[_ token] (->> exception
                            Throwable->map
                            :cause
                            (re-matches #".*Invalid keyword: (.+\/)."))]
    (let [token-pattern (re-pattern (str ":" token "(\\s|\\n|\\))"))]
      (when-let [[_ extra-token] (re-find token-pattern text)]
        (let [replaced-node (-> text
                                (string/replace-first token-pattern (str ":" token "_" extra-token))
                                z/of-string)]
          (if (z/find-next-value replaced-node z/next (keyword (str token "_")))
            (-> (z/edit-> replaced-node
                          (z/find-next-value z/next (keyword (str token "_")))
                          (z/replace (n/keyword-node (keyword token))))
                z/up)
            (-> (z/edit-> replaced-node
                          (z/find-next-token z/next #(= (str "::" token "_") (z/string %)))
                          (z/replace (n/keyword-node (keyword (str ":" token)))))
                z/up)))))))

(defn ^:private safe-zloc-of-string [text]
  (try
    (z/of-string text)
    (catch clojure.lang.ExceptionInfo e
      (if-let [node (handle-end-slash-code text e)]
        node
        (if-let [node (handle-keyword-with-end-slash-code text e)]
          node
          (if-let [node (handle-single-colon-code text e)]
            node
            (throw e)))))))

(defn loc-at-pos [code row col]
  (some-> code
          safe-zloc-of-string
          (edit/find-last-by-pos {:row row :col col :end-row row :end-col col})))

(defn safe-loc-at-pos [text row col]
  (try
    (loc-at-pos text row (dec col))
    (catch Exception _e
      (log/warn "It was not possible to parse cursor location, probably a not valid clojure text"))))

(defn safe-cursor-loc [uri line character db]
  (try
    (-> (get-in @db [:documents uri])
        :text
        (loc-at-pos (inc line) (inc character)))
    (catch Exception _
      (log/warn "It was not possible to get cursor location at given position. Probably a not valid clojure code"))))

(defn lein-zloc->edn [zloc]
  (when-let [zloc (some-> zloc
                          (z/find-next-value z/next 'defproject)
                          z/remove ;; remove defproject
                          z/down
                          z/remove ;; remove project name
                          z/down
                          z/remove ;; remove version
                          )]
    (z/sexpr (z/replace zloc (n/map-node (n/children (z/node zloc)))))))
