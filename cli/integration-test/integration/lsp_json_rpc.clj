(ns integration.lsp-json-rpc
  "Models LSP JSON-RPC as core.async channels of messages (Clojure hashmaps).

  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#baseProtocol"
  (:require
   [cheshire.core :as json]
   [clojure.core.async :as async]
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

(defn ^:private read-n-chars [^java.io.Reader reader content-length]
  (let [cs (char-array content-length)]
    (loop [total-read 0]
      (when (< total-read content-length)
        ;; FIXME: this is buggy. It reads `content-length` chars, but
        ;; `content-length` is specified in bytes. It works as long as the
        ;; integration tests are all in ASCII, but would break if they weren't.
        ;; See https://github.com/mainej/lsp4clj/blob/lsp2clj/server/src/lsp4clj/json_rpc.clj
        ;; for a correct implementation. The best way to fix this is probably to
        ;; wait for that to be merged and then use those helpers.
        (let [new-read (.read reader cs total-read (- content-length total-read))]
          (when (< new-read 0)
            ;; TODO: return nil instead?
            (throw (java.io.EOFException.)))
          (recur (+ total-read new-read)))))
    (String. cs)))

(defn ^:private parse-header [line headers]
  (let [[h v] (string/split line #":\s*" 2)]
    (when-not (contains? #{"Content-Length" "Content-Type"} h)
      (throw (ex-info "unexpected header" {:line line})))
    (assoc headers h v)))

(defn ^:private read-message [reader headers]
  (let [content-length (parse-long (get headers "Content-Length"))
        ;; TODO: handle content-type
        content (read-n-chars reader content-length)]
    (json/parse-string content true)))

(defn ^:private write-message [msg]
  (let [content (json/generate-string msg)]
    ;; FIXME: this is buggy. It sets `Content-Length` to the number of chars in
    ;; the content, but `Content-Length` should be the number of bytes (encoded
    ;; as UTF-8). The fix is the same as for `read-n-chars`: we should use the
    ;; helpers provided by lsp4clj.
    (print (str "Content-Length: " (.length content) "\r\n"
                "\r\n"
                content))
    (flush)))

(defn ^:private read-line-async
  "Reads a line of input asynchronously. Returns a channel which will yield the
  line when it is ready, or nil if the input has closed. Returns immediately.
  Avoids blocking by reading in a separate thread."
  [^java.io.BufferedReader input]
  ;; we are agnostic about \r\n or \n because readLine is too
  (async/thread (.readLine input)))

(defn buffered-reader->receiver-chan
  "Returns a channel which will yield parsed messages that have been read off
  the reader. When the reader is closed, closes the channel."
  [^java.io.BufferedReader reader]
  (let [msgs (async/chan 1)]
    (async/go-loop [headers {}]
      (if-let [line (async/<! (read-line-async reader))]
        (if (string/blank? line) ;; a blank line after the headers indicate start of message
          (do (async/>! msgs (read-message reader headers))
              (recur {}))
          (recur (parse-header line headers)))
        ;; input closed; also close channel
        (async/close! msgs)))
    msgs))

(defn buffered-writer->sender-chan
  "Returns a channel which expects to have messages put on it. nil values are
  not allowed. Serializes and writes the messages to the writer. When the
  channel is closed, closes the writer."
  [^java.io.BufferedWriter writer]
  (let [messages (async/chan 1)]
    (binding [*out* writer]
      (async/go-loop []
        (if-let [msg (async/<! messages)]
          (do
            (write-message msg)
            (recur))
          ;; channel closed; also close writer
          (.close writer))))
    messages))

(defn json-rpc-message
  ([method params] ;; notification
   {:jsonrpc "2.0"
    :method method
    :params params})
  ([id method params] ;; request
   (assoc (json-rpc-message method params) :id id)))
