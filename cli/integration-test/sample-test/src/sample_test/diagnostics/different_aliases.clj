(ns sample-test.diagnostics.different-aliases
    {:clj-kondo/config '{:linters {:duplicate-require {:level :off}}}}
    (:require [clojure.string]
              [clojure.string :as s]
              [clojure.string :as str]
              [clojure.string :as string]
              [clojure.string :as cstring]))
