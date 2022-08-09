(ns integration.helper
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as string]
   [clojure.test :refer [is]]))

(def root-project-path
  (-> (io/file *file*)
      .getParentFile
      .getParentFile
      .toPath
      (.resolve "sample-test")
      str))

(defn project-path->abs-path [path]
  (.getAbsolutePath (io/file root-project-path path)))

(defn source-path->file [source-path]
  (->> source-path
       (str "integration-test/sample-test/src/sample_test/")
       io/as-relative-path
       io/file))

(defn file->uri [file]
  (-> file .toPath .toUri .toString))

(defn source-path->uri [source-path]
  (->> source-path
       source-path->file
       file->uri))

(defn assert-submap [expected actual]
  (is (= expected
         (some-> actual (select-keys (keys expected))))
      (str "Actual:\n\n" (pr-str actual) "\nExpected:\n\n" (pr-str expected))))

(defmacro assert-submaps
  "Asserts that maps are submaps of result in corresponding order and
  that the number of maps corresponds to the number of
  results. Returns true if all assertions passed (useful for REPL).

   taken from kondo"
  [maps result]
  `(let [maps# ~maps
         res# ~result]
     (and
       (is (= (count maps#) (count res#))
           (format "Expected %s results, but got: %s \n--\n%s--"
                   (count maps#) (count res#) (with-out-str (pprint/pprint res#))))
       (doseq [[r# m#] (map vector res# maps#)]
         (assert-submap m# r#)))))

(defmacro assert-contains-submaps
  "Asserts that maps are contained submaps of result in results. "
  [maps result]
  `(let [maps# ~maps
         res# ~result]
     (doseq [m# maps#]
       (let [m-keys (keys m#)]
         (is (some (fn [element#] (= m# (some-> element# (select-keys m-keys)))) res#))))))

(defn delete-folder
  "Recursively delete a directory."
  [^String filename]
  (let [file ^java.io.File (io/file filename)]
    (when (.isDirectory file)
      (run! delete-folder (.listFiles file)))
    (io/delete-file file true)))

(defn delete-project-file
  [path]
  (->> path
       (str "integration-test/sample-test/src/sample_test/")
       io/as-relative-path
       delete-folder))

(defn code [& strings]
  (string/join \newline strings))
