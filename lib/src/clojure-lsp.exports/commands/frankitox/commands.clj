(ns frankitox.commands
  (:require
   [clj-kondo.core :as kondo]
   [clj-kondo.hooks-api :as clj-kondo.api]
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.feature.diagnostics :as f.diagnostic]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.refactor.transform :as r.transform]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.set :as set]
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
            in-edges (get graph sym)
            new-edges-count (reduce (fn [edges-count in-edge]
                                      (update edges-count in-edge dec))
                                    edges-count
                                    in-edges)]
        (recur
         new-edges-count
         (concat (rest zeros-queue)
                 (->> new-edges-count
                      (filter (fn [[sym c]]
                                (and (contains? in-edges sym)
                                     (zero? c))))
                      (map key)))
         (cons sym order)))
      order)))

;; sci seems to be missing this fn
(defn update-vals1 [m f]
  (into {} (map (fn [[k v]]
                  [k (f v)]))
        m))

(defn direct-deps [deps]
  (->> (update-vals1 deps z/of-node)
       (reduce-kv (fn [g sym zval]
                    (->> (keys deps)
                         (filter (fn [other-sym]
                                   (z/find-value zval z/next other-sym)))
                         (set)
                         (assoc g sym)))
                  {})))

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

(def mocks-map
  (read-string "{db (im.db/_d)
                 dbs (im.db/all-dbs)
                 user-id (im.models.user.queries/user-by-email \"alice@example.com\")
                 request-id \"req-id\"
                 permissions (im.models.permission.queries/all-user-perms-as-map user)
                 organization (im.models.user.queries/user-by-email \"alice@example.com\")
                 user (datomic.api/entity db user-id)
                 date-now (im.utils/date-now!)}"))

(defn mock-symbols* [initial-zloc findings]
  (let [top-zloc (edit/to-top initial-zloc)
        top-range (-> top-zloc z/node meta shared/->range)
        undefined? (->> findings
                        (filter (fn [finding]
                                  (= (:code finding)
                                     "unresolved-symbol")))
                        (filter (fn [{{:keys [start end]} :range}]
                                  ;; no character check as I'm analyzing top expressions
                                  (and (<= (get-in top-range [:start :line])
                                           (:line start))
                                       (<= (:line end)
                                           (get-in top-range [:end :line])))))
                        (mapv (fn [{:keys [message]}]
                                (symbol (subs message (count "Unresolved symbol: ")))))
                        (set))
        initial-undef-mocks (filter-keys undefined? mocks-map)]
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
            syms-to-define (filter-keys (comp not let-sym?) initial-undef-mocks)]
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
      (let [sym->deps (direct-deps (update-vals1 mocks-map n/coerce))
            undef-mocks (loop [collected-syms #{}
                               syms-to-check (set (keys initial-undef-mocks))]
                          (if (empty? syms-to-check)
                            collected-syms
                            (let [sym (first syms-to-check)]
                              (recur (conj collected-syms sym)
                                     (set/union (disj syms-to-check sym)
                                                (get sym->deps sym))))))
            sorted-mocks (reduce (fn [mocks sym]
                                   (if (contains? undef-mocks sym)
                                     (conj mocks [sym (get mocks-map sym)])
                                     mocks))
                                 []
                                 (reverse (topo-khan (deps-as-graph (update-vals1 mocks-map n/coerce)))))
            let-vec (reduce (fn [let-vec [idx [sym v]]]
                              (cond-> let-vec
                                (not (zero? idx))
                                (conj (n/spaces 4))
                                true
                                (conj (n/coerce sym)
                                      (n/spaces 1)
                                      (n/coerce v))
                                (not= (count sorted-mocks)
                                      (inc idx))
                                (conj (n/newlines 1))))
                            [] (map-indexed list sorted-mocks))]
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
        as-alias (z/sexpr (z/next (z/find-value (z/up (z/up loc)) z/next :as)))
        fq-sym (symbol (name as-alias) (name referred-sym))
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

(defonce *loc (atom nil))
(defn inner-remove-unused-bindings [loc]
  (println "Hello")
  (reset! *loc loc)
  [loc loc])

(defn remove-unused-bindings [{:keys [loc]}]
  (let [[old-loc new-zloc] (inner-remove-unused-bindings loc)]
    [{:range (meta (z/node old-loc))
      :loc new-zloc}]))

(defn print-loc [loc]
  (println
   (n/string
    (z/node
     (z/up loc)))))

(def tests-dir "file:///home/self/repos/clojure-lsp/tests/")

(defn substitute [cmd-response initial-str]
  (let [ranges (->> cmd-response
                    (r.transform/locs-to-ranges))
        {:keys [new-text] {:keys [start end]} :range} (first ranges)
        lines-idxs (loop [lines-idxs [0 (string/index-of initial-str "\n")]]
                     (if-let [idx (string/index-of initial-str "\n" (inc (last lines-idxs)))]
                       (recur (conj lines-idxs idx))
                       lines-idxs))]
    (str (subs initial-str 0 (+ (nth lines-idxs (:line start))
                                (:character start)))
         new-text
         (subs initial-str (+ (nth lines-idxs (:line end))
                              (:character end))))))

(comment
  (mapv
   (fn [test-str]
     (let [config (-> (str tests-dir test-str ".config.edn")
                      (slurp)
                      (edn/read-string))
           before-file (str tests-dir test-str ".before.clj")
           before-code (slurp before-file)
           initial-zloc (-> (parser/z-of-string* before-code)
                            (z/find-value z/next (:cursor-at-symbol config)))
           findings (->> (kondo/run! {:lint [before-file]})
                         (:findings)
                         (mapv (fn [finding]
                                 (#'clojure-lsp.feature.diagnostics/kondo-finding->diagnostic
                                  :full
                                  (delay (string/split before-code #"\r?\n"))
                                  nil
                                  finding))))]
       (let [[old-loc new-zloc] (mock-symbols* initial-zloc findings)]
         {test-str (= (substitute [{:range (meta (z/node old-loc))
                                    :loc new-zloc}]
                                  before-code)
                      (slurp (str tests-dir test-str ".after.clj")))})))
   ["db" "dependencies"]))
