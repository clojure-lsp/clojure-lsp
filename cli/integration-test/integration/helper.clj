(ns integration.helper
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as string]
   [clojure.test :refer [is]])
  (:import
   [java.net
    URLDecoder]
   [java.nio.charset StandardCharsets]))

(def windows?
  "Whether is running on MS-Windows."
  (string/starts-with? (System/getProperty "os.name") "Windows"))

(def ^:dynamic *escape-uris?* false)

(def root-project-path
  (-> (io/file *file*)
      .getParentFile
      .getParentFile
      (fs/path "sample-test")
      fs/canonicalize
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

(defn unescape-uri
  [^String uri]
  (try
    (URLDecoder/decode uri (.name StandardCharsets/UTF_8)) ;; compatible with Java 1.8 too!
    (catch IllegalArgumentException _
      uri)))

(defn escape-uri
  "Escapes enough URI characters for testing purposes and returns it.

  On MS-Windows, it will also escape the drive colon, mimicking
  VS-Code/Calva's behavior."
  [uri]
  ;; Do a better escape considering more chars
  (cond-> (string/replace uri "::" "%3A%3A")
    windows?
    (string/replace  #"/([a-zA-Z]):/" "/$1%3A/")))

(defn file->uri [file]
  (let [uri (-> file fs/canonicalize .toUri .toString)]
    (if *escape-uris?*
      (escape-uri uri)
      uri)))

(defn string=
  "Like `clojure.core/=` applied on STRING1 and STRING2, but treats
  any line endings as equal."
  [string1 string2]
  (= (string/split-lines string1) (string/split-lines string2)))

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
