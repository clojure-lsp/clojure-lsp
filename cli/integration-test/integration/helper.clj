(ns integration.helper
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as string]
   [clojure.test :refer [is]]))

(def windows?
  "Whether is running on MS-Windows."
  (string/starts-with? (System/getProperty "os.name") "Windows"))

(def root-project-path
  (-> (io/file *file*)
      .getParentFile
      .getParentFile
      .toPath
      (.resolve "sample-test")
      str))

(defn project-path->canon-path
  "Returns the canonical name of the root project's SUB-PATH."
  [sub-path]
  (.getCanonicalPath (io/file root-project-path sub-path)))

(defn source-path->file [source-path]
  (->> source-path
       (str "integration-test/sample-test/src/sample_test/")
       io/as-relative-path
       io/file))

(defn file-path
  "Converts the IMAGINARY-UNIX-TEST-PATH to something equivalent for the
  underlying system, and returns it.

  - On MS-Windows, it converts `/` to `\\` whewre appropriate while
  using a capital C: drive letter.

  - On all other systems, it return the input as is."
  [imaginery-unix-test-path]
  (cond-> imaginery-unix-test-path windows?
          (-> (string/replace-first #"^/" "C:\\\\")
              (->> (re-matches #"(.+?)(\.jar:.*)?"))
              (update 1 string/replace "/" "\\")
              rest
              (->> (apply str)))))

(defn file->uri [file]
  (-> file .toPath .toUri .toString))

(defn string=
  "Like `clojure.core/=` applied on STRING1 and STRING2, but treats
  any line endings as equal."
  [string1 string2]
  (= (string/split-lines string1) (string/split-lines string2)))

(defn ->system-newlines
  "Converts TEXT new lines to those of the underlying system, and
  returns it."
  [text]
  (-> (string/split-lines text) (string/join "\n")))

(defn str-includes?
  "Like `clojure.string/includes?` applied to S and SUBSTR, but treats
  any line endings as equal."
  [s substr]
  (let [s (-> (string/split-lines s) (string/join "\n"))
        substr (-> (string/split-lines substr) (string/join "\n"))]
    (string/includes? s substr)))

(defn newlines->system
  "Converts TEXT new lines to those of the underlying system, and
  returns it."
  [text]
  (-> (string/split-lines text) (string/join (System/lineSeparator))))

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
