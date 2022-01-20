(ns clojure-lsp.classpath-lookup
  (:require
   [clojure.java.io :as io]
   [clojure.string :refer [ends-with?]]
   [taoensso.timbre :as log])
  (:import
   [java.io ByteArrayOutputStream File InputStream]
   [java.util.jar JarFile JarEntry]))

(defn- file? [path]
  (.isFile (io/as-file path)))

(defn- path-to-jar-file? [path]
  (and (file? path)
       (ends-with? path ".jar")))

(defn- jar-entry-is-file? [^JarEntry entry]
  (not (.isDirectory entry)))

(defn- slurp-bytes [^InputStream in]
  (with-open [^ByteArrayOutputStream out (ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn- jar-entries [^String filepath]
  (with-open [^JarFile jar (JarFile. filepath)]
    (->> (enumeration-seq (.entries jar))
         (filter (fn [^JarEntry entry] (not (.isDirectory entry))))
         (mapv (fn [^JarEntry config-entry]
                 (.getName config-entry))))))

(defn- extract-files-from-jar
  "Extracts files from a jar.
   
   Returns tuples in the form [filepath byte-array] where filepath is the path
   of a file contained in a jar."
  [jar-file-path]
  (with-open [^JarFile jar (JarFile. ^String jar-file-path)]
    (->> (enumeration-seq (.entries jar))
         (filter jar-entry-is-file?)
         (mapv (fn [^JarEntry entry]
                 [(.getName entry)
                  (with-open [in (.getInputStream jar entry)]
                    (slurp-bytes in))])))))

(defn- new-jar-index
  "Builds an unexpanded jar index.
   
   A jar index is basically an index of files found in jars in a classpath.
   It's a map where keys are file paths (found in jars) to their respective
   contents (in bytes).

   A new jar index, though, starts with file paths mapped to the path of the
   containing jar file. We do that to speed up the first lookup by avoiding to
   eagerly extract all jar files in the classpath at once. When a file is first
   looked up in the index, and if its contents is a byte array, we return its
   contents. But if it's a string, we 'expand' the file by reading it from the
   jar and updating the index with the file bytes."
  [classpath]
  (->> classpath
       (filter path-to-jar-file?)
       (mapcat (fn [jar-path]
                 (map #(vector % jar-path) (jar-entries jar-path))))
       (into {})))

(defn- expand-file-in-jar-index?
  "When `filepath` at `jar-index` is a string it means the file has not yet
   been expanded in the jar index. In such case the string is a path to the jar
   file that contains the file. That path can be used to read the file bytes
   from the jar and replace the jar file path in the index with the file bytes.
   
   Also returns false when `filepath` is not found in the jar index."
  [jar-index filepath]
  (string? (jar-index filepath)))

(defn- expand-file-in-jar-index
  "Expands a file in the jar index, if it's not been expanded yet."
  [jar-index filepath]
  (if-not (expand-file-in-jar-index? jar-index filepath)
    jar-index
    (->> (jar-index filepath)
         (extract-files-from-jar)
         (into jar-index))))

(defn- dir-seq
  "Like file-seq but for directories."
  [dir]
  (tree-seq
   (fn [^File f] (. f (isDirectory)))
   (fn [^File d] (filter (fn [^File f] (. f (isDirectory)))
                         (. d (listFiles))))
   (io/as-file dir)))

(defn- lookup-source-file [filepath source-paths]
  (some->> source-paths
           (mapcat dir-seq)
           (map #(io/file % filepath))
           (filter file?)
           (first)
           (io/input-stream)
           (slurp-bytes)))

(defn- lookup-file [filepath source-paths jar-index]
  (or (get jar-index filepath)
      (lookup-source-file filepath source-paths)))

(defn- expand-file-in-jar-index! [filepath db]
  (->> (expand-file-in-jar-index (:jar-index @db) filepath)
       (swap! db assoc :jar-index)
       :jar-index))

(defn- create-jar-index! [db]
  (log/info "creating jar index")
  (swap! db assoc :jar-index (new-jar-index (:classpath @db))))

(defn lookup! [filepath db]
  (when-not (:jar-index @db)
    (create-jar-index! db))
  (let [jar-index (expand-file-in-jar-index! filepath db)
        source-paths (-> @db :settings :source-paths)]
    (lookup-file filepath source-paths jar-index)))
