(ns clojure-lsp.feature.clojuredocs
  "clojuredocs integration inspired on orchard implementation."
  (:require
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.edn :as edn]
   [lsp4clj.protocols.logger :as logger])
  (:import
   (java.io IOException)
   (java.net URL)
   (javax.net.ssl HttpsURLConnection)))

(set! *warn-on-reflection* true)

(def ^:private clojuredocs-edn-file-url
  "https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn")

(def ^:private connect-timeout-ms 1000)
(def ^:private read-timeout-ms 30000)

(defn ^:private test-remote-url [^String url]
  (if-not (.startsWith url "http")
    [true]
    (let [url (URL. url)
          conn ^HttpsURLConnection (.openConnection url)]
      (.setConnectTimeout conn connect-timeout-ms)
      (.setReadTimeout conn read-timeout-ms)
      (try
        (.connect conn)
        [true]
        (catch IOException ex
          [false ex])
        (finally
          (.disconnect conn))))))

(defn refresh-cache! [db logger]
  (when (and (settings/get db [:hover :clojuredocs] true)
             (not (-> @db :clojuredocs :refreshing?)))
    (logger/info (:logger @db) "Refreshing clojuredocs cache...")
    (swap! db assoc-in [:clojuredocs :refreshing?] true)
    (shared/logging-time
      logger
      "Refreshing clojuredocs cache took %s secs."
      (try
        (let [;; connection check not to wait too long
              [downloadable? conn-ex] (test-remote-url clojuredocs-edn-file-url)]
          (if (not downloadable?)
            (logger/error (:logger @db) "Could not refresh clojuredocs." conn-ex)
            (swap! db assoc :clojuredocs {:cache (-> clojuredocs-edn-file-url
                                                     slurp
                                                     edn/read-string)})))
        (catch Exception e
          (logger/error (:logger @db) "Error refreshing clojuredocs information." e)
          nil)
        (finally
          (swap! db assoc-in [:clojuredocs :refreshing?] false))))))

(defn find-docs-for [sym-name sym-ns db logger]
  (when sym-ns
    (let [full-keyword (keyword (str sym-ns) (str sym-name))]
      (if-let [cache (-> @db :clojuredocs :cache)]
        (get cache full-keyword)
        (do
          (async/go
            (refresh-cache! db logger))
          nil)))))

(defn find-hover-docs-for [sym-name sym-ns db logger]
  (when (settings/get db [:hover :clojuredocs] true)
    (find-docs-for sym-name sym-ns db logger)))
