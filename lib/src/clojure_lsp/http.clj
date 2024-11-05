(ns clojure-lsp.http
  (:import
   (java.io IOException)
   (java.net URL)
   (javax.net.ssl HttpsURLConnection)))

(set! *warn-on-reflection* true)

(def ^:private default-connect-timeout-ms 1000)
(def ^:private default-read-timeout-ms 30000)

(defn test-remote-url! [^String url]
  (if-not (.startsWith url "http")
    [true]
    (let [url (URL. url)
          conn ^HttpsURLConnection (.openConnection url)]
      (.setConnectTimeout conn default-connect-timeout-ms)
      (.setReadTimeout conn default-read-timeout-ms)
      (try
        (.connect conn)
        [true]
        (catch IOException ex
          [false ex])
        (finally
          (.disconnect conn))))))

(defn request! [^String url]
  (try
    (let [url (URL. url)
          conn ^HttpsURLConnection (.openConnection url)]
      (.setConnectTimeout conn default-connect-timeout-ms)
      (.setReadTimeout conn default-read-timeout-ms)
      {:status (.getResponseCode conn)
       :body (.getInputStream conn)
       :content-type (.getContentType conn)})
    (catch Exception ex
      {:error ex})))
