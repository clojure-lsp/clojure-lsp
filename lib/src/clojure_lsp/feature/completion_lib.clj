(ns clojure-lsp.feature.completion-lib
  (:require
   [cheshire.core :as json]
   [clojure-lsp.http :as http]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.deps.extensions :as tools.deps.extensions]
   [clojure.tools.deps.util.maven :as tools.deps.maven]
   [rewrite-clj.zip :as z])
  (:import
   [clojure.lang PersistentVector]
   [java.util.zip GZIPInputStream]))

(set! *warn-on-reflection* true)

(def ^:private deps-files
  {"deps.edn" :clojure-deps
   "bb.edn" :babashka
   "project.clj" :leiningen})

(defn lib-completion-context
  "Completion of version of libs in deps.edn, project.clj files."
  [cursor-loc uri]
  (when-let [dep-type (and cursor-loc (get deps-files (.getName (io/file uri))))]
    (case dep-type
      (:clojure-deps :babashka)
      (cond
        ;; inside :mvn/version string
        (identical? :mvn/version (when (some-> cursor-loc z/prev z/tag (= :token))
                                   (some-> cursor-loc z/prev z/sexpr)))
        #:dep{:type :version
              :coordinate :mvn/version
              :lib (z/sexpr (z/prev (z/up cursor-loc)))}

        ;; inside :git/tag string
        (identical? :git/tag (when (some-> cursor-loc z/prev z/tag (= :token))
                               (some-> cursor-loc z/prev z/sexpr)))
        #:dep{:type :version
              :coordinate :git/tag
              :lib (z/sexpr (z/prev (z/up cursor-loc)))}

        ;; inside {} without coordinate
        (some-> cursor-loc z/prev z/string symbol qualified-symbol?)
        #:dep{:type :version
              :lib (some-> cursor-loc z/prev z/string symbol)}

        ;; inside {} without coordinate
        (and (some-> cursor-loc z/prev z/prev z/sexpr-able?)
             (some-> cursor-loc z/prev z/prev z/sexpr symbol?)
             (some-> cursor-loc z/prev z/prev z/string symbol qualified-symbol?))
        #:dep{:type :version
              :lib (some-> cursor-loc z/prev z/prev z/string symbol)}

        ;; lib name
        (and (some-> cursor-loc z/prev z/prev z/sexpr-able?)
             (contains? #{:deps :extra-deps :replace-deps}
                        (some-> cursor-loc z/up z/prev z/sexpr)))
        #:dep{:type :name})

      :leiningen
      (cond
        ;; lib version
        (and (contains? #{:dependencies :managed-dependencies :plugins :pom-plugins :extensions}
                        (some-> cursor-loc z/up z/up z/prev z/sexpr))
             (some-> cursor-loc z/sexpr string?))
        #:dep{:type :version
              :coordinate :mvn/version
              :lib (some-> cursor-loc z/leftmost z/sexpr symbol)}

        ;; lib name
        (or (contains? #{:dependencies :managed-dependencies :plugins :pom-plugins :extensions}
                       (some-> cursor-loc z/up z/prev z/sexpr))
            (contains? #{:dependencies :managed-dependencies :plugins :pom-plugins :extensions}
                       (some-> cursor-loc z/up z/up z/prev z/sexpr)))
        #:dep{:type :name}))))

(defn ^:private complete-lib-version [{:dep/keys [coordinate lib]} matches-fn]
  (let [versions-coord (tools.deps.extensions/find-all-versions lib nil {:mvn/repos tools.deps.maven/standard-repos})
        version-fn #(or (when coordinate (get % coordinate)) (:git/tag %) (:mvn/version %))
        versions (map version-fn versions-coord)
        item-fn (fn [version label]
                  (when (or (not coordinate)
                            (matches-fn label))
                    (let [index (.indexOf ^PersistentVector (vec (reverse versions)) version)]
                      {:label label
                       :detail (when (= 0 index) "latest")
                       :sort-text (format "%03d" index)
                       :kind :text
                       :priority :lib-version})))]
    (if coordinate
      (keep (fn [version] (item-fn version version)) versions)
      (keep (fn [version-coord]
              (let [version-coord-str (str version-coord)]
                (item-fn (version-fn version-coord) (subs version-coord-str 1 (dec (count version-coord-str))))))
            versions-coord))))

(defonce libs* (atom nil))

(defn ^:private fetch-clojars-libs! []
  (try
    (->> "https://clojars.org/repo/all-jars.clj.gz"
         io/input-stream
         GZIPInputStream.
         io/reader
         line-seq
         (keep #(first (try (edn/read-string %) (catch Exception _ nil))))
         dedupe
         (map (fn [lib]
                (if (simple-symbol? lib)
                  (symbol (str lib) (str lib))
                  lib))))
    (catch Exception _
      [])))

(defn ^:private get-mvn-artifacts!
  "All the artifacts under group-id in mvn central"
  [group-id]
  (let [search-prefix "https://search.maven.org/solrsearch/select?q=g:%22"
        search-suffix "%22+AND+p:%22jar%22&rows=2000&wt=json"
        search-url (str search-prefix group-id search-suffix)
        {:keys [body status error]} (http/request! search-url)]
    (if (or error (not= 200 status))
      []
      (->> (with-open [r (io/reader body)]
             (json/parse-stream r keyword))
           :response
           :docs
           (keep :a)))))

(defn ^:private fetch-clojure-mvn-central-libs []
  (->> ["org.clojure" "com.cognitect"]
       (pmap (fn [group-id]
               (->> group-id
                    get-mvn-artifacts!
                    (mapv (fn [artifact]
                            (symbol group-id artifact))))))
       (reduce into [])))

(defn ^:private all-clojure-libs!
  "Fetch clojars libs and org.clojure only libs in mvn-central, caching it."
  []
  (or @libs*
      (let [clojars-libs* (future (fetch-clojars-libs!))
            mvn-central-libs* (future (fetch-clojure-mvn-central-libs))]
        (reset! libs* (into @clojars-libs* @mvn-central-libs*)))))

(defn ^:private complete-lib-name [_lib-context cursor-value]
  (let [libs (all-clojure-libs!)]
    (keep (fn [lib-name]
            (when (string/starts-with? lib-name (str cursor-value))
              {:label (str lib-name)
               :kind :text
               :priority :lib-name})) libs)))

(defn complete [lib-context matches-fn cursor-value]
  (case (:dep/type lib-context)
    :version
    (complete-lib-version lib-context matches-fn)

    :name
    (complete-lib-name lib-context cursor-value)

    nil))
