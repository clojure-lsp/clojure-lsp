(ns clojure-lsp.feature.clojuredocs
  "clojuredocs integration inspired on orchard implementation."
  (:require
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.async :as async]
   [clojure.edn :as edn]
   [taoensso.timbre :as log])
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

(defn refresh-cache! [db]
  (when (and (settings/get db [:hover :clojuredocs] true)
             (not (-> @db :clojuredocs :refreshing?)))
    (log/info "Refreshing clojuredocs cache...")
    (swap! db assoc-in [:clojuredocs :refreshing?] true)
    (shared/logging-time
      "Refreshing clojuredocs cache took %s secs."
      (try
        (let [;; connection check not to wait too long
              [downloadable? conn-ex] (test-remote-url clojuredocs-edn-file-url)]
          (if (not downloadable?)
            (log/error "Could not refresh clojuredocs." conn-ex)
            (swap! db assoc :clojuredocs {:cache (-> clojuredocs-edn-file-url
                                                     slurp
                                                     edn/read-string)})))
        (catch Exception e
          (log/error "Error refreshing clojruedocs." e)
          nil)
        (finally
          (swap! db assoc-in [:clojuredocs :refreshing?] false))))))

(defn find-docs-for [sym-name sym-ns db]
  (when (and sym-ns
             (settings/get db [:hover :clojuredocs] true))
    (let [full-keyword (keyword (str sym-ns) (str sym-name))]
      (if-let [cache (-> @db :clojuredocs :cache)]
        (get cache full-keyword)
        (do
          (async/go
            (refresh-cache! db))
          nil)))))
