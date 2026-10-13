(ns clojure-lsp.feature.paredit
  (:require
   [clojure-lsp.logger :as logger]
   [clojure-lsp.refactor.edit :as edit]
   [rewrite-clj.paredit :as paredit]
   [rewrite-clj.zip :as z]))

(defn ^:private move-range [zloc z-move-fn]
  (let [sexpr-zloc (if (z/sexpr-able? zloc) zloc (z-move-fn zloc))]
    (meta (z/node sexpr-zloc))))

(defn ^:private move-sexp-or-up-range [zloc row col z-move-fn edge-position-fn]
  (when zloc
    (let [current-range (meta (z/node zloc))
          sexpr? (z/sexpr-able? zloc)
          at-edge? (and sexpr?
                        (= [row col] (edge-position-fn current-range)))
          target-zloc (if (and sexpr? (not at-edge?))
                        zloc
                        (or (z-move-fn zloc)
                            (let [parent-zloc (z/up zloc)]
                              (when (and parent-zloc
                                         (not= :forms (z/tag parent-zloc)))
                                parent-zloc))))]
      (some-> target-zloc z/node meta))))

(defn forward [uri zloc _row _col]
  (let [range (move-range zloc z/right)]
    {:show-document-after-edit {:uri uri
                                :take-focus true
                                :range (assoc range
                                              :row (:end-row range)
                                              :col (:end-col range))}}))

(defn forward-sexp-or-up
  "Moves forward by sexpr, moving out of the enclosing form at its end."
  [uri zloc row col]
  (if-let [range (move-sexp-or-up-range zloc row col z/right (juxt :end-row :end-col))]
    {:show-document-after-edit {:uri uri
                                :take-focus true
                                :range (assoc range
                                              :row (:end-row range)
                                              :col (:end-col range))}}
    {:no-op? true}))

(defn forward-select [uri zloc row col]
  (let [range (move-range zloc z/right)]
    {:show-document-after-edit {:uri uri
                                :take-focus true
                                :range (assoc range
                                              :row row
                                              :col col)}}))

(defn backward [uri zloc _row _col]
  (let [range (move-range zloc z/left)]
    {:show-document-after-edit {:uri uri
                                :take-focus true
                                :range (assoc range
                                              :end-row (:row range)
                                              :end-col (:col range))}}))

(defn backward-sexp-or-up
  "Moves backward by sexpr, moving out of the enclosing form at its beginning."
  [uri zloc row col]
  (if-let [range (move-sexp-or-up-range zloc row col z/left (juxt :row :col))]
    {:show-document-after-edit {:uri uri
                                :take-focus true
                                :range (assoc range
                                              :end-row (:row range)
                                              :end-col (:col range))}}
    {:no-op? true}))

(defn backward-select [uri zloc row col]
  (let [range (move-range zloc z/left)]
    {:show-document-after-edit {:uri uri
                                :take-focus true
                                :range (assoc range
                                              :end-row row
                                              :end-col col)}}))

(defn ^:private paredit-op [paredit-fn move-cursor? uri original-zloc row col]
  (try
    (when original-zloc
      (let [{original-row :row original-col :col} (-> original-zloc z/node meta)
            offset-row (- row original-row)
            offset-col (- col original-col)
            pos-zloc (-> original-zloc
                         z/root-string
                         (z/of-string {:track-position? true}) ;; https://clojurians.slack.com/archives/CHB5Q2XUJ/p1740958982303849
                         (edit/find-at-pos row col))
            zloc (paredit-fn pos-zloc)
            [row' col'] (z/position zloc)
            [new-row new-col] (if (= (z/node original-zloc) (z/node zloc)) ;; if the node where the cursor is has changed
                                [(+ row' offset-row) (+ col' offset-col)] ;; move the cursor to the new node position
                                [row col]) ;; otherwise keep the cursor in the same place
            root-zloc (z/up (edit/to-top zloc))]
        {:changes-by-uri {uri [{:loc root-zloc
                                :range (meta (z/node root-zloc))}]} ;; FIXME: range is always the whole document
         :show-document-after-edit {:uri uri
                                    :take-focus true
                                    :range (if move-cursor?
                                             (let [range (meta (z/node (z/up original-zloc)))]
                                               (assoc range
                                                      :end-row (:row range)
                                                      :end-col (:col range)))
                                             {:row new-row
                                              :col new-col
                                              :end-row new-row
                                              :end-col new-col})}}))
    (catch Exception e
      (logger/error e))))

(def forward-slurp (partial paredit-op #(paredit/slurp-forward-into % {:from :current}) false))
(def forward-barf (partial paredit-op paredit/barf-forward false))
(def backward-slurp (partial paredit-op #(paredit/slurp-backward-into % {:from :current}) false))
(def backward-barf (partial paredit-op paredit/barf-backward false))
(def raise (partial paredit-op paredit/raise true))
(def kill (partial paredit-op paredit/kill false))
