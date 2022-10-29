(ns performance.helper
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]))

(def file (io/file *file*))

(defn project->abs-path [project]
  (-> file
      .getParentFile
      .getParentFile
      (fs/path project)
      fs/canonicalize
      str))
