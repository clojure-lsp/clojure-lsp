(ns pod.clojure-lsp.api
  {:no-doc true}
  (:refer-clojure :exclude [read read-string])
  (:require
   [bencode.core :as bencode]
   [clojure-lsp.api :as api]
   [cognitect.transit :as transit])
  (:import
   [java.io PushbackInputStream])
  (:gen-class))

(set! *warn-on-reflection* true)

(def stdin (PushbackInputStream. System/in))

(defn write [v]
  (bencode/write-bencode System/out v)
  (.flush System/out))

(defn read-string [^"[B" v]
  (String. v))

(defn read []
  (bencode/read-bencode stdin))

;; transit

;;; payload
(def jiofile-key (str ::file))

(def jiofile-read-handler
  (transit/read-handler (fn [^String s] (java.io.File. s))))

(def jiofile-write-handler
  (transit/write-handler jiofile-key str))

(defn reg-transit-handlers
  []
  (format "
(require 'babashka.pods)
(babashka.pods/add-transit-read-handler!
    \"%s\"
    (fn [s] (java.io.File. s)))
(babashka.pods/add-transit-write-handler!
  #{java.io.File}
  \"%s\"
  str)
"
          jiofile-key jiofile-key))

(def transit-read-handlers
  (delay
    (transit/read-handler-map
      {jiofile-key jiofile-read-handler})))

(def transit-write-handlers
  (delay
    (transit/write-handler-map
      {java.io.File jiofile-write-handler})))

(defn read-transit [^String v]
  (transit/read
    (transit/reader
      (java.io.ByteArrayInputStream. (.getBytes v "utf-8"))
      :json
      {:handlers @transit-read-handlers})))

(defn write-transit [v]
  (let [baos (java.io.ByteArrayOutputStream.)]
    (transit/write
      (transit/writer
        baos
        :json
        {:handlers @transit-write-handlers}) v)
    (.toString baos "utf-8")))

(def api-vars
  {'clojure-lsp.api/analyze-project! api/analyze-project!
   'clojure-lsp.api/clean-ns! api/clean-ns!
   'clojure-lsp.api/diagnostics api/diagnostics
   'clojure-lsp.api/format! api/format!
   'clojure-lsp.api/rename! api/rename!})

(defn run-pod []
  (loop []
    (let [message (try (read)
                       (catch java.io.EOFException _
                         ::EOF))]
      (when-not (identical? ::EOF message)
        (let [op (-> (get message "op")
                     read-string
                     keyword)
              id (some-> (get message "id")
                         read-string
                         (or "unknown"))]
          (case op
            :describe (do (write {"format" "transit+json"
                                  "namespaces" [{:name "pod.clojure-lsp.api"
                                                 :vars [{:name '-reg-transit-handlers
                                                         :code (reg-transit-handlers)}]}
                                                {"name" "clojure-lsp.api"
                                                 "vars" (->> api-vars
                                                             keys
                                                             (mapv (fn [k] {"name" (name k)})))}]
                                  "id" id})
                          (recur))
            :invoke (do (try
                          (let [var (-> (get message "var")
                                        read-string
                                        symbol)
                                args (some-> (get message "args")
                                             read-string
                                             read-transit)]
                            (if-let [f (api-vars var)]
                              (let [value (-> (binding [*out* *err*]
                                                (apply f args))
                                              write-transit)
                                    reply {"value" value
                                           "id" id
                                           "status" ["done"]}]
                                (write reply))
                              (throw (ex-info (str "Var not found: " var) {}))))
                          (catch Throwable e
                            (binding [*out* *err*]
                              (println e))
                            (let [reply {"ex-message" (.getMessage e)
                                         "ex-data" (write-transit
                                                     (assoc (ex-data e)
                                                            :type (class e)))
                                         "id" id
                                         "status" ["done" "error"]}]
                              (write reply))))
                        (recur))
            (do
              (write {"err" (str "unknown op:" (name op))})
              (recur))))))))
