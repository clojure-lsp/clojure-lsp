(ns rewrite-clj.custom-zipper.core
  ; (:require-macros [rewrite-clj.custom-zipper.core :refer [defn-switchable]])
  (:require [rewrite-clj.node.protocols :as node]
            [clojure.zip :as clj-zip]))

; (defn-switchable insert-right
;   "Inserts the item as the right sibling of the node at this loc,
;   without moving"
;   [loc item]
;   (let [{:keys [parent right]} loc]
;     (if-not parent
;       (throw (new Exception "Insert at top"))
;       (assoc loc
;              :changed? true
;              :right (cons item right)))))
