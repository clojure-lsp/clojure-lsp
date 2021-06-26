(ns clojure-lsp.parser
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.refactor.edit :as edit]
    [clojure.string :as string]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]
    [taoensso.timbre :as log]))

(defmacro zspy [loc]
  `(do
     (log/warn '~loc (pr-str (z/sexpr ~loc)))
     ~loc))

(defn z-next-sexpr [loc]
  (z/find-next loc z/next #(not (n/printable-only? (z/node %)))))

(defn ident-split [ident-str]
  (let [ident-conformed (some-> ident-str (string/replace #"^::?" ""))
        prefix (string/replace ident-str #"^(::?)?.*" "$1")
        idx (string/index-of ident-conformed "/")]
    (if (and idx (not= idx (dec (count ident-conformed))))
      (into [prefix] (string/split ident-conformed #"/" 2))
      [prefix nil ident-conformed])))

;; From rewrite-cljs
(defn in-range? [{:keys [row col end-row end-col] :as form-pos}
                 {r :row c :col er :end-row ec :end-col :as selection-pos}]
  (or (nil? form-pos)
      (nil? selection-pos)
      (and (>= r row)
           (<= er end-row)
           (if (= r row) (>= c col) true)
           (if (= er end-row) (< ec end-col) true))))

(defn same-range? [{:keys [name-row name-col name-end-row name-end-col] :as _a-pos}
                   {r :name-row c :name-col er :name-end-row ec :name-end-col :as _b-pos}]
  (and (= r name-row)
       (= er name-end-row)
       (= c name-col)
       (= ec name-end-col)))

;; From rewrite-cljs
(defn find-forms
  "Find last node (if more than one node) that is in range of pos and
  satisfying the given predicate depth first from initial zipper
  location."
  [zloc p?]
  (->> zloc
       (iterate z-next-sexpr)
       (take-while identity)
       (take-while (complement z/end?))
       (filter p?)))

(defn find-last-by-pos
  [zloc pos]
  (let [forms (find-forms zloc (fn [loc]
                                 (in-range?
                                  (-> loc z/node meta) pos)))
        disconsider-reader-macro? (and (some #(= "?" (z/string %)) forms)
                                       (> (count forms) 1))]
    (if disconsider-reader-macro?
      (last (filter (complement (comp #(= "?" %) z/string)) forms))
      (last forms))))

(defn find-top-forms-in-range
  [code pos]
  (->> (find-forms (z/of-string code) #(in-range? pos (-> % z/node meta)))
       (mapv (fn [loc] (z/find loc z/up edit/top?)))
       (distinct)))

(defn ^:private safe-zloc-of-string [text]
  (try
    (z/of-string text)
    (catch clojure.lang.ExceptionInfo e
      (if-let [[_ token] (->> e
                              Throwable->map
                              :cause
                              (re-matches #"Invalid symbol: (.*\/)."))]
        (let [token-pattern (re-pattern (str token "(\\s|\\n|\\))"))]
          (if-let [[_ extra-token] (re-find token-pattern text)]
            (-> text
                (string/replace-first token-pattern (str token "_" extra-token))
                z/of-string
                (z/edit->
                  (z/find-next-value z/next (symbol (str token "_")))
                  (z/replace (n/token-node (symbol token))))
                z/up)
            (throw e)))
        (throw e)))))

(defn loc-at-pos [code row col]
  (some-> code
          safe-zloc-of-string
          (find-last-by-pos {:row row :col col :end-row row :end-col col})))

(defn safe-loc-at-pos [text row col]
  (try
    (loc-at-pos text row (dec col))
    (catch Exception _e
      (log/warn "It was not possible to parse cursor location, probably a not valid clojure text"))))

(defn safe-cursor-loc [uri line character]
  (try
    (-> @db/db
        (get-in [:documents uri])
        :text
        (loc-at-pos (inc line) (inc character)))
    (catch Exception _
      (log/warn "It was not possible to get cursor location at given position. Probably a not valid clojure code"))))
