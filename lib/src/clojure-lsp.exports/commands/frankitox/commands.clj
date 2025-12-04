(ns frankitox.commands
  (:require
   [clj-kondo.hooks-api :as clj-kondo.api]
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn get-pos [zloc]
  (-> zloc z/node meta))

(defn join-positions
  [{:keys [col row]}
   {:keys [end-col end-row]}]
  {:col col
   :row row
   :end-col end-col
   :end-row end-row})

(defn mapcat-left [f coll]
  (mapcat (fn [idx elem]
            (if (zero? idx)
              (list elem)
              (f (nth coll (dec idx)) elem)))
          (range (count coll))
          coll))

(defn mapcat-children [f node]
  (if-let [children (seq (:children node))]
    (assoc node :children (map (fn [new-child]
                                 (mapcat-children f new-child))
                               (f children)))
    node))

(defn indent-right [children]
  (mapcat-left (fn [prev-child child]
                 (cond
                   (and (:newlines prev-child)
                        (:whitespace child))
                   (list (update child :whitespace str "  "))
                   (:newlines prev-child)
                   (list (n/spaces 2) child)
                   :else
                   (list child)))
               children))

(defn indent-children [node]
  (mapcat-children indent-right node))

(defn map-kv [f coll]
  (into {} (map (fn [[k v]]
                  (f k v))) coll))

(defn filter-keys [pred coll]
  (into {} (filter (fn [[k _v]]
                     (pred k)))
        coll))

;; https://chatgpt.com/share/68f6f0cd-7c34-8008-b895-56ea25aa7cf3
(defn topo-khan [graph]
  (loop [edges-count (map-kv (fn [sym _required-by]
                               [sym (->> (dissoc graph sym)
                                         (filter (fn [[_sym' depends-on]]
                                                   (contains? depends-on sym)))
                                         (count))])
                             graph)
         zeros-queue (->> (seq edges-count)
                          (filter (comp zero? second))
                          (map key))
         order '()]
    (if (seq zeros-queue)
      (let [sym (first zeros-queue)
            new-order (cons sym order)
            in-edges (get graph sym)
            new-edges-count (reduce (fn [edges-count in-edge]
                                      (update edges-count in-edge dec))
                                    edges-count
                                    in-edges)]
        (recur
          new-edges-count
          (rest zeros-queue)
          (concat new-order
                  (->> new-edges-count
                       (filter (fn [[sym c]]
                                 (and (contains? in-edges sym)
                                      (zero? c))))
                       (map key)))))
      order)))

;; sci seems to be missing this fn
(defn update-vals1 [m f]
  (into {} (map (fn [[k v]]
                  [k (f v)]))
        m))

(defn deps-as-graph [deps]
  (let [zdeps (update-vals1 deps z/of-node)]
    (reduce (fn [graph sym]
              (->> (reduce-kv (fn [s dep-sym zval]
                                (if (z/find-value zval z/next sym)
                                  (conj s dep-sym)
                                  s))
                              #{} zdeps)
                   (assoc graph sym)))
            {} (keys deps))))

(comment
  (let [deps (read-string "{db (im.db/_d)
                          dbs (im.db/all-dbs)
                          user-id (im.models.user.queries/user-by-email \"alice@example.com\")
                          request-id \"req-id\"
                          permissions (im.models.permission.queries/all-user-perms-as-map user)
                          organization (im.models.user.queries/user-by-email \"alice@example.com\")
                          user (datomic.api/entity db user-id)
                          date-now (im.utils/date-now!)}")
        zdeps (update-vals deps (comp z/of-node n/coerce))]
    (topo-khan (deps-as-graph (update-vals deps n/coerce)))
    #_(reduce (fn [graph sym]
                (->> (reduce-kv (fn [s dep-sym zval]
                                  (if (z/find-value zval z/next sym)
                                    (conj s dep-sym)
                                    s))
                                #{} zdeps)
                     (assoc graph sym)))
              {} (keys deps))))

;; {var -> node}

;; [x] find every var that appear in my magic map '{db (im.db/_d)}
;; [x] wrap everything in a let
;; [x] analyze which vars are undefined
;;   clojure-lsp.feature.diagnostics/kondo-findings->diagnostics
;; [ ] create a dep tree for the magic map so new vars can refer old vars?
;; [ ] move `magic-map` to file
;; [ ] fix case (comment ...) without a root let
(defn mock-symbols* [initial-zloc findings]
  (let [top-zloc (edit/to-top initial-zloc)
        mocks-map (read-string "{db (im.db/_d)
                                 dbs (im.db/all-dbs)
                                 coll-db (im.db/_d \"photos\")
                                 user-id (im.models.user.queries/user-by-email \"alice@example.com\")
                                 request-id \"req-id\"
                                 permissions (im.models.permission.queries/all-user-perms-as-map user)
                                 organization (im.models.user.queries/user-by-email \"alice@example.com\")
                                 org (im.models.user.queries/user-by-email \"alice@example.com\")
                                 user (datomic.api/entity db (im.models.user.queries/user-by-email \"alice@example.com\"))
                                 date-now (im.utils/date-now!)}")
        top-range (-> top-zloc z/node meta shared/->range)
        undefined? (->> findings
                        (filter (fn [finding]
                                  (= (:code finding)
                                     "unresolved-symbol")))
                        (filter (fn [{{:keys [start end]} :range}]
                                  (and (<= (get-in top-range [:start :line])
                                           (:line start))
                                       (<= (get-in top-range [:start :character])
                                           (:character start))
                                       (<= (:line end)
                                           (get-in top-range [:end :line]))
                                       (<= (:character end)
                                           (get-in top-range [:end :character])))))
                        (mapv (fn [{:keys [message]}]
                                (symbol (subs message (count "Unresolved symbol: ")))))
                        (set))
        available-mocks (filter-keys undefined? mocks-map)]
    (if (edit/inside-rcf? top-zloc)
      (let [let-sym? (->> (z/node top-zloc)
                          (:children)
                          (filter clj-kondo.api/list-node?)
                          (first)
                          (:children)
                          (filter clj-kondo.api/vector-node?)
                          (first)
                          (n/sexpr)
                          (map-indexed list)
                          (filter (fn [[idx]]
                                    (even? idx)))
                          (mapv second)
                          (set))
            letv-zloc (z/next (z/find-value top-zloc z/next 'let))
            syms-to-define (filter-keys (comp not let-sym?) available-mocks)]
        [top-zloc
         (edit/to-top
           (z/replace letv-zloc
                      (update (z/node letv-zloc) :children concat
                              (mapcat (fn [[sym v]]
                                        [(n/newlines 1)
                                         (n/spaces 8)
                                         (n/coerce sym)
                                         (n/spaces 1)
                                         (n/coerce v)])
                                      syms-to-define))))])
      (let [let-vec (reduce (fn [let-vec [idx [sym v]]]
                              (cond-> let-vec
                                (not (zero? idx))
                                (conj (n/spaces 4))
                                true
                                (conj (n/coerce sym)
                                      (n/spaces 1)
                                      (n/coerce v))
                                (not= (count available-mocks)
                                      (inc idx))
                                (conj (n/newlines 1))))
                            [] (map-indexed list available-mocks))]
        [top-zloc
         (z/replace top-zloc
                    (indent-children
                      (n/list-node
                        [(n/token-node 'comment)
                         (n/newlines 1)
                         (indent-children
                           (n/list-node
                             (concat [(n/token-node 'let)
                                      (n/spaces 1)]
                                     [(n/vector-node
                                        let-vec)]
                                     [(n/newlines 1)
                                      (z/node top-zloc)])))])))]))))

(defn print-loc [loc]
  (println
    (n/string
      (z/node
        (z/up loc)))))

(comment
  (let [filename "file:///home/self/repos/clojure-lsp/already-defined-db"
        ;; "file:///home/self/bfa/im/src/clj/im/db.clj"
        initial-zloc (-> (#'clojure-lsp.parser/zloc-of-string
                          (clojure-lsp.shared/slurp-uri filename))
                         (rewrite-clj.zip/find-value rewrite-clj.zip/next 'db))
        findings (->> (clj-kondo.core/run! {:lint [filename]})
                      (:findings)
                      (mapv (fn [finding]
                              (#'clojure-lsp.feature.diagnostics/kondo-finding->diagnostic
                               :full
                               (delay (-> (slurp filename)
                                          (string/split #"\r?\n")))
                               nil
                               finding))))]
    (let [top-zloc (edit/to-top initial-zloc)
          mocks-map (read-string "{db (im.db/_d)
                                 dbs (im.db/all-dbs)
                                 coll-db (im.db/_d \"photos\")
                                 user-id (im.models.user.queries/user-by-email \"alice@example.com\")
                                 request-id \"req-id\"
                                 permissions (im.models.permission.queries/all-user-perms-as-map user)
                                 organization (im.models.user.queries/user-by-email \"alice@example.com\")
                                 org (im.models.user.queries/user-by-email \"alice@example.com\")
                                 user (datomic.api/entity db (im.models.user.queries/user-by-email \"alice@example.com\"))
                                 date-now (im.utils/date-now!)}")
          top-range (-> top-zloc z/node meta shared/->range)
          undefined? (->> findings
                          (filter (fn [finding]
                                    (= (:code finding)
                                       "unresolved-symbol")))
                          (filter (fn [{{:keys [start end]} :range}]
                                    (and (<= (get-in top-range [:start :line])
                                             (:line start))
                                         (<= (get-in top-range [:start :character])
                                             (:character start))
                                         (<= (:line end)
                                             (get-in top-range [:end :line]))
                                         (<= (:character end)
                                             (get-in top-range [:end :character])))))
                          (mapv (fn [{:keys [message]}]
                                  (symbol (subs message (count "Unresolved symbol: ")))))
                          (set))
          available-mocks (filter-keys undefined? mocks-map)]
      (if (edit/inside-rcf? top-zloc)

        (let [let-sym? (->> (z/node top-zloc)
                            (:children)
                            (filter clj-kondo.api/list-node?)
                            (first)
                            (:children)
                            (filter clj-kondo.api/vector-node?)
                            (first)
                            (n/sexpr)
                            (map-indexed list)
                            (filter (fn [[idx]]
                                      (even? idx)))
                            (mapv second)
                            (set))
              letv-zloc (z/next (z/find-value top-zloc z/next 'let))
              syms-to-define (filter-keys (comp not let-sym?) available-mocks)]
          [top-zloc
           (edit/to-top
             (z/replace letv-zloc
                        (update (z/node letv-zloc) :children concat
                                (mapcat (fn [[sym v]]
                                          [(n/newlines 1)
                                           (n/spaces 8)
                                           (n/coerce sym)
                                           (n/spaces 1)
                                           (n/coerce v)])
                                        syms-to-define))))])
        (when-let [l (edit/find-ops-up top-zloc "let")]
          [available-mocks (z/node (z/next l))])
        #_(let [let-vec (reduce (fn [let-vec [idx [sym v]]]
                                  (cond-> let-vec
                                    (not (zero? idx))
                                    (conj (n/spaces 4))
                                    true
                                    (conj (n/coerce sym)
                                          (n/spaces 1)
                                          (n/coerce v))
                                    (not= (count available-mocks)
                                          (inc idx))
                                    (conj (n/newlines 1))))
                                [] (map-indexed list available-mocks))]
            [top-zloc
             (z/replace top-zloc
                        (indent-children
                          (n/list-node
                            [(n/token-node 'comment)
                             (n/newlines 1)
                             (indent-children
                               (n/list-node
                                 (concat [(n/token-node 'let)
                                          (n/spaces 1)]
                                         [(n/vector-node
                                            let-vec)]
                                         [(n/newlines 1)
                                          (z/node top-zloc)])))])))])))
    #_(let [[old-loc new-zloc] (mock-symbols* initial-zloc findings)]
        (r.transform/locs-to-ranges
          [{:range (meta (z/node old-loc))
            :loc new-zloc}]))
    #_(print-loc
        (second (mock-symbols* zloc findings)))
    #_(print-loc
        (z/node
          (z/up
            (mock-symbols zloc))))))

(defn skip-left-whitespace [zloc]
  (loop [zloc zloc]
    (if (n/whitespace? (z/node (z/prev* zloc)))
      (recur (z/prev* zloc))
      zloc)))

(defn add-require [loc args]
  (let [[ns-sym alias-sym] (->> (take 2 args)
                                (mapv symbol))]
    (f.add-missing-libspec/add-to-namespace*
      loc
      {:type :require :lib ns-sym :alias alias-sym}
      {})))

(defn underef [{:keys [loc]}]
  (let [referred-sym (z/sexpr loc)
        alias (z/sexpr (z/next (z/find-value (z/up (z/up loc)) z/next :as)))
        fq-sym (symbol (name alias) (name referred-sym))
        zloc-after-ns (z/right (z/up (z/up (z/up (z/up loc)))))
        vector-zloc (let [prev-zloc (z/remove* loc)]
                      (if (not (clj-kondo.api/vector-node? (z/node prev-zloc)))
                        (z/up prev-zloc)
                        prev-zloc))
        trim-refer-pos (if (every? n/whitespace? (n/children (z/node vector-zloc)))
                         (join-positions
                           (-> (z/find-value vector-zloc z/prev :refer)
                               (skip-left-whitespace)
                               (get-pos))
                           (get-pos vector-zloc))
                         (let [wzloc (skip-left-whitespace loc)]
                           (join-positions
                             (get-pos wzloc)
                             (get-pos loc))))]
    (concat [{:range trim-refer-pos
              :new-text ""}]
            (loop [zloc zloc-after-ns
                   changes []]
              (if-let [new-zloc (z/find-value zloc z/next referred-sym)]
                (recur (z/next new-zloc)
                       (conj changes {:range (meta (z/node new-zloc))
                                      :loc (z/of-string (str fq-sym))}))
                changes)))))

(defn mock-symbols [{:keys [uri loc db]}]
  (let [[old-loc new-zloc] (mock-symbols* loc (f.diagnostic/find-diagnostics uri db))]
    [{:range (meta (z/node old-loc))
      :loc new-zloc}]))

#_(defn substitute [cmd-response file-str]
    (let [ranges (->> cmd-response
                      (r.transform/locs-to-ranges))
          {:keys [new-text] {:keys [start end]} :range} (first ranges)
          lines-idxs (loop [lines-idxs [0 (string/index-of file-str "\n")]]
                       (if-let [idx (string/index-of file-str "\n" (inc (last lines-idxs)))]
                         (recur (conj lines-idxs idx))
                         lines-idxs))]
      (str (subs file-str 0 (+ (nth lines-idxs (:line start))
                               (inc (:character start))))
           new-text
           (subs file-str (+ (nth lines-idxs (:line end))
                             (inc (:character end)))))))

#_(comment
    (let [initial-string "(ns coco)\n(do db)\ndbs"
          initial-zloc (-> (#'clojure-lsp.parser/zloc-of-string initial-string)
                           (rewrite-clj.zip/find-value rewrite-clj.zip/next 'db))
          cmd-response (mock-symbols {:loc initial-zloc})]
      (substitute cmd-response initial-string)))
