(ns clojure-lsp.parser
  (:require
   [clojure-lsp.logger :as logger]
   [clojure-lsp.refactor.edit :as edit]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro zspy [loc]
  `(do
     (taoensso.timbre/warn '~loc (pr-str (z/sexpr ~loc)))
     ~loc))

(defn same-range? [{:keys [name-row name-col name-end-row name-end-col] :as _a-pos}
                   {r :name-row c :name-col er :name-end-row ec :name-end-col :as _b-pos}]
  (and (= r name-row)
       (= er name-end-row)
       (= c name-col)
       (= ec name-end-col)))

(def ^:private zero-width-space
  "A unicode character that is incredibly unlikely to be used in regular code.
  During parsing, used as a valid and easily identified subsitute for what would
  otherwise be an invalid character. This character was chosen because
  rewrite-clj parses it as a single character in a symbol, not as whitespace,
  and because a zero-width space is invisible and so has little use in source
  code."
  "\u200b")

(defn ^:private replace-incomplete-token [s invalid-str valid-str]
  (let [token-pattern (re-pattern (str invalid-str "(\\s|\\n|\\)|\\]|\\})"))
        matcher (re-matcher token-pattern s)]
    (loop [[_ divider] (re-find matcher)
           new-s s]
      (if divider
        (recur (re-find matcher)
               (string/replace-first new-s token-pattern (str valid-str divider)))
        new-s))))

(defn z-of-string* [s]
  (some-> s p/parse-string-all (z/of-node* {})))

(defn ^:private z-replace-preserving-meta [zloc replacement]
  (z/replace zloc (with-meta replacement (meta (z/node zloc)))))

(defn ^:private handle-end-slash-code [text exception]
  (when-let [[_ token] (->> exception
                            Throwable->map
                            :cause
                            (re-matches #"Invalid symbol: (.*)\/."))]
    (let [real-value      (str token "/")
          temporary-value (str token zero-width-space)]
      (some-> text
              (replace-incomplete-token real-value temporary-value)
              z-of-string*
              (z/edit->
                (z/find-value z/next (symbol temporary-value))
                (z-replace-preserving-meta (n/token-node (symbol real-value))))))))

(defn ^:private handle-single-colon-code [text exception]
  (let [cause (->> exception Throwable->map :cause)]
    (when (or (re-matches #"\[line (\d+), col (\d+)\] A single colon is not a valid keyword." cause)
              (re-matches #"\[line (\d+), col (\d+)\] Invalid keyword: ." cause))
      (let [real-value      ":"
            temporary-value zero-width-space]
        (some-> text
                (replace-incomplete-token real-value temporary-value)
                z-of-string*
                (z/edit->
                  (z/find-value z/next (symbol temporary-value))
                  (z-replace-preserving-meta (n/token-node (symbol real-value)))))))))

(defn ^:private handle-keyword-with-end-slash-code [text exception]
  (when-let [[_ token] (->> exception
                            Throwable->map
                            :cause
                            (re-matches #".*Invalid keyword: (.+)\/."))]
    (let [real-value      (str token "/")
          temporary-value (str token zero-width-space)]
      (when-let [replaced-node (some-> text
                                       (replace-incomplete-token (str ":" real-value)
                                                                 (str ":" temporary-value))
                                       z-of-string*)]
        (if (z/find-value replaced-node z/next (keyword temporary-value))
          (z/edit-> replaced-node
                    (z/find-value z/next (keyword temporary-value))
                    (z-replace-preserving-meta (n/keyword-node (keyword real-value))))
          (z/edit-> replaced-node
                    (z/find-token z/next #(= (str "::" temporary-value) (z/string %)))
                    (z-replace-preserving-meta (n/keyword-node (keyword (str ":" real-value))))))))))

(defn ^:private zloc-of-string [text]
  (try
    (z-of-string* text)
    (catch clojure.lang.ExceptionInfo e
      (or (handle-end-slash-code text e)
          (handle-keyword-with-end-slash-code text e)
          (handle-single-colon-code text e)
          (throw e)))))

(defn safe-zloc-of-string [text]
  (try
    (zloc-of-string text)
    (catch Exception _e
      (logger/warn "It was not possible to parse text. Probably not valid clojure code."))))

(defn zloc-of-file [db uri]
  (zloc-of-string (get-in db [:documents uri :text])))

(defn safe-zloc-of-file [db uri]
  (try
    (zloc-of-file db uri)
    (catch Exception _e
      (logger/warn "It was not possible to parse file. Probably not valid clojure code."))))

(defn safe-zloc-sexpr
  "Parse sexpr handling not valid codes that are zlocs
   but not sexpr-able like `[{:}]`"
  [zloc]
  (when (z/sexpr-able? zloc)
    (try
      (z/sexpr zloc)
      (catch IllegalArgumentException _
        nil))))

(defn to-pos [zloc row col]
  (edit/find-at-pos zloc row col))

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
