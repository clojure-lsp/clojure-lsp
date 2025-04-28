(ns clojure-lsp.feature.selection-range
  (:require
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [rewrite-clj.zip :as z]))

(defn ^:private range->inner-range [range]
  (-> range
      (update :col inc)
      (update :end-col dec)))

(defn ^:private zloc->selection-range [zloc]
  (loop [cur-zloc zloc
         ranges-tree []]
    (let [new-tree (conj ranges-tree (meta (z/node cur-zloc)))]
      (if (edit/top? cur-zloc)
        (reduce
          (fn [acc range]
            (shared/assoc-some
              {:range (shared/->range range)}
              :parent acc))
          nil
          (drop-last (reverse (interleave (map range->inner-range new-tree)
                                       new-tree))))
        (recur (z/up* cur-zloc) new-tree)))))

(defn selection-ranges [uri positions components]
  (let [[row col] (first positions)
        zloc (some-> (f.file-management/force-get-document-text uri components)
                     parser/safe-zloc-of-string
                     (parser/to-pos row col))]
    [(zloc->selection-range zloc)]))
