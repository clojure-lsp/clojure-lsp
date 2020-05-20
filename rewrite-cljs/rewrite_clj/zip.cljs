(ns rewrite-clj.zip
  "Client facing facade for zipper functions"
  (:refer-clojure :exclude [next find replace remove
                            seq? map? vector? list? set?
                            print map get assoc])
  (:require [rewrite-clj.zip.base :as base]
            [rewrite-clj.parser :as p]
            [rewrite-clj.zip.move :as m]
            [rewrite-clj.zip.findz :as f]
            [rewrite-clj.zip.editz :as ed]
            [rewrite-clj.zip.insert :as ins]
            [rewrite-clj.zip.removez :as rm]
            [rewrite-clj.zip.seqz :as sz]
            [clojure.zip :as z]))



(def node
  "Function reference to clojure.zip/node"
  z/node)
(def root
  "Function reference to clojure.zip/root"
  z/root)


(def of-string
  "See [[base/of-string]]"
  base/of-string)
(def root-string
  "See [[base/root-string]]"
  base/root-string)
(def string
  "See [[base/string]]"
  base/string)
(def tag
  "See [[base/tag]]"
  base/tag)
(def sexpr
  "See [[base/sexpr]]"
  base/sexpr)




;; **********************************
;; Originally in rewrite-clj.zip.move
;; **********************************
(def right
  "See [[move/right]]"
  m/right)
(def left
  "See [[move/left]]"
  m/left)
(def down
  "See [[move/down]]"
  m/down)
(def up
  "See [[move/up]]"
  m/up)
(def next
  "See [[move/next]]"
  m/next)
(def end?
  "See [[move/end?]]"
  m/end?)
(def rightmost?
  "See [[move/rightmost?]]"
  m/rightmost?)
(def leftmost?
  "See [[move/leftmost?]]"
  m/leftmost?)
(def prev
  "See [[move/prev]]"
  m/prev)
(def leftmost
  "See [[move/leftmost]]"
  m/leftmost)
(def rightmost
  "See [[move/rightmost]]"
  m/rightmost)



;; **********************************
;; Originally in rewrite-clj.zip.findz
;; **********************************
(def find
  "See [[findz/find]]"
  f/find)
(def find-last-by-pos
  "See [[findz/find-last-by-pos]]"
  f/find-last-by-pos)
(def find-depth-first
  "See [[findz/find-depth-first]]"
  f/find-depth-first)
(def find-next
  "See [[findz/find-next]]"
  f/find-next)
(def find-next-depth-first
  "See [[findz/find-next-depth-first]]"
  f/find-next-depth-first)
(def find-tag
  "See [[findz/find-tag]]"
  f/find-tag)
(def find-next-tag
  "See [[findz/find-next-tag]]"
  f/find-next-tag)
(def find-tag-by-pos
  "See [[findz/tag-by-pos]]"
  f/find-tag-by-pos)
(def find-token
  "See [[findz/find-token]]"
  f/find-token)
(def find-next-token
  "See [[findz/find-next-token]]"
  f/find-next-token)
(def find-value
  "See [[findz/find-value]]"
  f/find-value)
(def find-next-value
  "See [[findz/find-next-value]]"
  f/find-next-value)



;; **********************************
;; Originally in rewrite-clj.zip.editz
;; **********************************
(def replace
  "See [[editz/replace]]"
  ed/replace)
(def edit
  "See [[editz/edit]]"
  ed/edit)
(def splice
  "See [[editz/splice]]"
  ed/splice)
(def prefix
  "See [[editz/prefix]]"
  ed/prefix)
(def suffix
  "See [[editz/suffix]]"
  ed/suffix)

;; **********************************
;; Originally in rewrite-clj.zip.removez
;; **********************************
(def remove
  "See [[removez/remove]]"
  rm/remove)
(def remove-preserve-newline
  "See [[removez/remove-preserve-newline]]"
  rm/remove-preserve-newline)


;; **********************************
;; Originally in rewrite-clj.zip.insert
;; **********************************
(def insert-right
  "See [[insert/insert-right]]"
  ins/insert-right)
(def insert-left
  "See [[insert/insert-left]]"
  ins/insert-left)
(def insert-child
  "See [[insert/insert-child]]"
  ins/insert-child)
(def append-child
  "See [[insert/append-child]]"
  ins/append-child)


;; **********************************
;; Originally in rewrite-clj.zip.seqz
;; **********************************
(def seq?
  "See [[seqz/seq?]]"
  sz/seq?)
(def list?
  "See [[seqz/list?]]"
  sz/list?)
(def vector?
  "See [[seqz/vector?]]"
  sz/vector?)
(def set?
  "See [[seqz/set?]]"
  sz/set?)
(def map?
  "See [[seqz/map?]]"
  sz/map?)
(def map-vals
  "See [[seqz/map-vals]]"
  sz/map-vals)
(def map-keys
  "See [[seqz/map-keys]]"
  sz/map-keys)
(def map
  "See [[seqz/map]]"
  sz/map)
(def get
  "See [[seqz/get]]"
  sz/get)
(def assoc
  "See [[seqz/assoc]]"
  sz/assoc)

(defn skip
  "Perform the given movement while the given predicate returns true."
  [f p? zloc]
  (->> (iterate f zloc)
       (take-while identity)
       (take-while (complement z/end?))
       (drop-while p?)
       (first)))
