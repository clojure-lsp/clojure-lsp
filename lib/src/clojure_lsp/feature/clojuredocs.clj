(ns clojure-lsp.feature.clojuredocs
  "clojuredocs integration inspired on orchard implementation."
  (:require
   [clojure-lsp.http :as http]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.edn :as edn]))

(set! *warn-on-reflection* true)

(def ^:private clojuredocs-logger-tag "[clojuredocs]")

(def ^:private clojuredocs-edn-file-url
  "https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn")

(defn refresh-cache! [db*]
  (when (and (settings/get @db* [:hover :clojuredocs] true)
             (not (-> @db* :clojuredocs :refreshing?)))
    (shared/logging-task
      :internal/refresh-clojuredocs-cache
      (try
        (swap! db* assoc-in [:clojuredocs :refreshing?] true)
        (let [;; connection check not to wait too long
              [downloadable? conn-ex] (http/test-remote-url! clojuredocs-edn-file-url)]
          (if (not downloadable?)
            (logger/error clojuredocs-logger-tag "Could not refresh clojuredocs." conn-ex)
            (swap! db* assoc :clojuredocs {:cache (-> clojuredocs-edn-file-url
                                                      slurp
                                                      edn/read-string)})))
        (catch Exception e
          (logger/error clojuredocs-logger-tag "Error refreshing clojuredocs information." e)
          nil)
        (finally
          (swap! db* assoc-in [:clojuredocs :refreshing?] false))))))

(defn find-docs-for [sym-name sym-ns db*]
  (when sym-ns
    (let [full-keyword (keyword (str sym-ns) (str sym-name))]
      (if-let [cache (-> @db* :clojuredocs :cache)]
        (get cache full-keyword)
        (do
          (async/go
            (refresh-cache! db*))
          nil)))))

(defn find-hover-docs-for [sym-name sym-ns db*]
  (when (settings/get @db* [:hover :clojuredocs] true)
    (find-docs-for sym-name sym-ns db*)))
