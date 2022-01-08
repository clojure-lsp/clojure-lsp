(ns clojure-lsp.feature.move-coll-entry
  (:require
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn- count-siblings-left
  "Count the number of sibling nodes to the left of the child node `zloc`."
  [zloc]
  (->> zloc
       (iterate z/left)
       (take-while (complement z/leftmost?))
       count))

(defn- count-siblings-right
  "Count the number of sibling nodes to the right of the child node `zloc`."
  [zloc]
  (->> zloc
       (iterate z/right)
       (take-while (complement z/rightmost?))
       count))

(defn ^:private expand-comment-newlines
  "Comment nodes often include their trailing newline. This splits such nodes
  into two, to allow more careful handling of newlines."
  [children]
  (mapcat (fn [{:keys [s prefix] :as n}]
            (if (and (n/comment? n)
                     (string/ends-with? s "\n"))
              [(n/comment-node prefix (subs s 0 (dec (count s))))
               (n/newline-node "\n")]
              [n]))
          children))

(defn- trailing-whitespace? [node]
  (and (n/whitespace-or-comment? node)
       (not (n/linebreak? node))))

(defn ^:private parse-entry-pairs
  "Parse the children of a map or binding `parent-zloc` into entry pairs
  (key/value pairs or binding/value pairs respectively).

  An entry pair is vector of nodes: the key and value themselves, with their
  surrounding whitespace, comments and newlines.

  Logically, a pair's format is:
  (whitespace | comment | newline)*
  key
  (whitespace | comment | newline)*
  value
  (whitespace | comment)*

  NOTE: A pair does NOT include any trailing newlines... they are all allocated
  to the next pair.

  Returns two values: a vector of the entry pairs, and if there are lines
  after the last value, a vector of those additional nodes."
  [parent-zloc]
  (let [[elements extra-lines]
        (loop [children (->> parent-zloc
                             z/node
                             n/children
                             expand-comment-newlines)
               state    :before-key
               result   []]
          (cond
            (not (seq children))
            ;; everything processed
            [result []]

            (every? n/whitespace-or-comment? children)
            ;; everything processed but extra lines
            [result children]

            (= :before-key state)
            ;; gather comments before key, and key itself
            (let [[prefix [k & rst]] (split-with n/whitespace-or-comment?
                                                 children)]
              (recur rst
                     :before-val
                     (conj result (concat prefix (when k [k])))))

            (= :before-val state)
            ;; gather comments before val, val itself, and optional comment
            ;; after val on same line
            (let [[prefix [v & rst]] (split-with n/whitespace-or-comment?
                                                 children)
                  [postfix rst]      (split-with trailing-whitespace?
                                                 rst)]
              (recur rst
                     :before-key
                     (conj result (concat prefix (when v [v]) postfix))))))]
    [(->> elements
          (partition-all 2)
          (mapv (fn [[k v]] (concat k v))))
     extra-lines]))

(defn ^:private move-entry-zloc
  "Move `zloc` to direction `dir` considering multiple comments cases."
  [zloc dir]
  (let [parent-zloc               (z/up zloc)
        [entry-pairs extra-lines] (parse-entry-pairs parent-zloc)

        node-idx     (count-siblings-left zloc)
        new-node-idx (dir (dir node-idx))
        ;; index of the entry pair we're moving, whether the original
        ;; node was a key or value.
        pair-idx     (-> node-idx (/ 2) int)
        new-pair-idx (dir pair-idx)
        entry-pairs  (assoc entry-pairs
                            pair-idx (get entry-pairs new-pair-idx)
                            new-pair-idx (get entry-pairs pair-idx))
        ;; Most entries can keep the whitespace that precedes them,
        ;; whether moving up or down. This assumption fails when
        ;; swapping the first and second entries. The entry that is
        ;; initially first typically doesn't have any preceding
        ;; whitespace, but it's moving to a location where it needs
        ;; some. And conversely, the entry that is initially second
        ;; typically has preceding whitespace but is moving somewhere
        ;; where it shouldn't. We fix this by moving preceding
        ;; whitespace from one to the other.
        ;; TODO: seems to work whether or not we're swapping the first
        ;; and second entries. But is that accounting for all whitespace
        ;; edge cases? Maybe better to do this only if (= #{0 1}
        ;; (hash-set pair-idx new-pair-idx)).
        [whitespace first-entry] (split-with n/whitespace? (get entry-pairs 0))
        entry-pairs              (-> entry-pairs
                                     (assoc 0 first-entry)
                                     (update 1 #(concat whitespace %)))

        ;; Put revised entries back into parent.
        parent-zloc (z/subedit-> parent-zloc
                                 (z/replace (n/replace-children (z/node parent-zloc)
                                                                (concat (flatten entry-pairs)
                                                                        extra-lines))))

        ;; If after reordering, the last component has become a comment,
        ;; we need to add a newline or else the closing bracket will be
        ;; commented out.
        last-zloc   (-> parent-zloc z/down z/rightmost*)
        parent-zloc (if (-> last-zloc z/node n/comment?)
                      ;; align bracket with last key
                      (let [last-key (-> parent-zloc z/down z/rightmost z/left)
                            col      (:col (meta (z/node last-key)))]
                        (-> last-zloc
                            (z/insert-space-right (dec col))
                            z/insert-newline-right
                            z/up))
                      parent-zloc)]
    ;; Move zipper back to original node, so repeated invocations keep
    ;; moving the same entry.
    (->> parent-zloc
         z/down
         (iterate z/right)
         (drop new-node-idx)
         first)))

(defn ^:private can-move-entry? [zloc]
  (and (contains? #{:map :vector} (some-> zloc z/up z/tag))
       (even? (count (z/child-sexprs (z/up zloc))))))

(defn can-move-entry-up? [zloc]
  (and (can-move-entry? zloc)
       (>= (count-siblings-left zloc) 2)))

(defn can-move-entry-down? [zloc]
  (and (can-move-entry? zloc)
       (>= (count-siblings-right zloc) 2)))

(defn ^:private move-entry [zloc uri dir focus-zloc]
  (when-let [new-zloc (move-entry-zloc zloc dir)]
    {:show-document-after-edit {:uri uri
                                :take-focus? true
                                :range (meta (z/node focus-zloc))}
     :changes-by-uri {uri
                      [{:range (meta (z/node (z/up new-zloc)))
                        :loc (z/up new-zloc)}]}}))

(defn move-up [zloc uri]
  (when (can-move-entry-up? zloc)
    (move-entry zloc uri dec (-> zloc z/left z/left))))

(defn move-down [zloc uri]
  (when (can-move-entry-down? zloc)
    (move-entry zloc uri inc (-> zloc z/right z/right))))
