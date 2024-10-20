(ns clojure-lsp.snapshot
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn ^:private parse [line]
  (let [[_ file row column level linter message]
        (re-find #"(.*):(\d+):(\d+): ([^:]+): \[([^\]]+)+] (.*)" line)
        [source code] (str/split linter #"/")]
    {:file file
     :row (dec (Integer. row))
     :column (dec (Integer. column))
     :level level
     :severity (case level
                 "error" 1
                 "warning" 2
                 "info" 3)
     :source source
     :code code
     :linter linter
     :message message
     :raw line}))

(defn read-db
  ([] (read-db ".lsp/snapshot.txt"))
  ([path]
   (let [file (io/file path)]
     (if (.exists file)
       (let [result (reduce (fn [acc line]
                              (let [{:keys [severity linter row column file]} (parse line)]
                                (update-in acc [severity linter row column] (fnil conj []) file)))
                            {}
                            (with-open [reader (io/reader file)]
                              (doall (line-seq reader))))]
         #_(spit "snapshot.edn" (pr-str result))
         result)
       {}))))

(defn has-diagnostic?
  [uri diagnostic snapshot]
  (let [{:keys [severity code source range]} diagnostic
        linter (if (or (= "clj-kondo" source)
                       (str/starts-with? code (str source "/")))
                 code
                 (str source "/" code))
        row (-> range :start :line)
        column (-> range :start :character)
        result (boolean (when-let [files (get-in snapshot [severity linter row column])]
                          (some #(str/includes? uri %) files)))]

    result))

(defn discard
  [uri diagnostics]
  (let [snapshot (read-db)]
    (remove #(has-diagnostic? uri % snapshot)
            diagnostics)))
