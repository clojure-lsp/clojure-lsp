(ns clojure-lsp.feature.format
  (:require
   [cljfmt.core :as cljfmt]
   [cljfmt.main :as cljfmt.main]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.memoize :as memoize]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(defn resolve-user-cljfmt-config [db]
  (let [config-path (settings/get db [:cljfmt-config-path] ".cljfmt.edn")
        project-root (shared/uri->filename (:project-root-uri @db))
        cljfmt-config-file (if (string/starts-with? config-path "/")
                             (io/file config-path)
                             (io/file project-root config-path))]
    (medley/deep-merge
      (settings/get db [:cljfmt] {})
      (when (shared/file-exists? cljfmt-config-file)
        (edn/read-string {:readers {'re re-pattern}} (slurp cljfmt-config-file))))))

(defn ^:private resolve-cljfmt-config [db]
  (cljfmt.main/merge-default-options
    (resolve-user-cljfmt-config db)))

(def memoize-ttl-threshold-milis 3000)

(def cljfmt-config
  (memoize/ttl resolve-cljfmt-config :ttl/threshold memoize-ttl-threshold-milis))

(defn formatting [uri db]
  (let [{:keys [text]} (get-in @db [:documents uri])
        cljfmt-settings (cljfmt-config db)
        new-text (cljfmt/reformat-string text cljfmt-settings)]
    (if (= new-text text)
      []
      [{:range (shared/full-file-range)
        :new-text new-text}])))

(defn range-formatting [doc-id format-pos db]
  (let [{:keys [text]} (get-in @db [:documents doc-id])
        cljfmt-settings (cljfmt-config db)
        forms (parser/find-top-forms-in-range text format-pos)]
    (mapv (fn [form-loc]
            {:range (shared/->range (-> form-loc z/node meta))
             :new-text (n/string (cljfmt/reformat-form (z/node form-loc) cljfmt-settings))})
          forms)))

(comment
  (require '[clj-async-profiler.core :as profiler])
  (require '[criterium.core :as bench])
  (require '[clojure-lsp.db :as db])
  (require '[clojure-lsp.refactor.edit :as edit])
  (defn range-formatting-non-reduce-map-reformat [doc-id format-pos db]
    (let [cljfmt-settings (cljfmt-config db)
          root-loc (parser/zloc-of-file @db doc-id)
          forms (edit/find-forms root-loc #(edit/in-range? format-pos (-> % z/node meta)))
          start-top-loc (-> forms first (z/find z/up edit/top?))
          end-top-loc (-> forms last (z/find z/up edit/top?))

          forms (->> start-top-loc
                     (iterate z/right*)
                     (take-while identity)
                     (take-while (complement z/end?))
                     (medley/take-upto #(= % end-top-loc)))]
      (mapv (fn [form-loc]
              {:range (shared/->range (-> form-loc z/node meta))
               :new-text (n/string (cljfmt/reformat-form (z/node form-loc) cljfmt-settings))})
            forms)))

  (defn range-formatting-non-reduce-merge-format [doc-id format-pos db]
    (let [cljfmt-settings (cljfmt-config db)
          root-loc (parser/zloc-of-file @db doc-id)
          forms (edit/find-forms root-loc #(edit/in-range? format-pos (-> % z/node meta)))
          start-top-loc (-> forms first (z/find z/up edit/top?))
          end-top-loc (-> forms last (z/find z/up edit/top?))

          forms (->> start-top-loc
                     (iterate z/right*)
                     (take-while identity)
                     (take-while (complement z/end?))
                     (medley/take-upto #(= % end-top-loc)))
          span (merge (-> start-top-loc z/node meta (select-keys [:row :col]))
                      (-> end-top-loc z/node meta (select-keys [:end-row :end-col])))]
      [{:range (shared/->range span)
        :new-text (-> (map z/node forms)
                      n/forms-node
                      (cljfmt/reformat-form cljfmt-settings)
                      n/string)}]))

  (defn range-formatting-custom-to-pos [doc-id format-pos db]
    (let [cljfmt-settings (cljfmt-config db)
          root-loc (parser/zloc-of-file @db doc-id)
          start-loc (or (parser/to-pos root-loc (:row format-pos) (:col format-pos))
                        (z/leftmost* root-loc))
          end-loc (or (parser/to-pos start-loc (:end-row format-pos) (:end-col format-pos))
                      (z/rightmost* root-loc))
          start-top-loc (z/find start-loc z/up edit/top?)
          end-top-loc (z/find end-loc z/up edit/top?)

          forms (->> start-top-loc
                     (iterate z/right*) ;; maintain comments and whitespace between nodes
                     (take-while identity)
                     (take-while (complement z/end?))
                     (medley/take-upto #(= % end-top-loc)))
          span (merge (-> start-top-loc z/node meta (select-keys [:row :col]))
                      (-> end-top-loc z/node meta (select-keys [:end-row :end-col])))]
      [{:range (shared/->range span)
        :new-text (-> (map z/node forms)
                      n/forms-node
                      (cljfmt/reformat-form cljfmt-settings)
                      n/string)}]))

  (defn emacs->pos [el ec]
    [el (inc ec)])

  (defn stub []
    (bench/quick-bench (* 2 3))
    (profiler/profile
      {:min-width 5}
      (dotimes [_ 500]
        (* 2 3))))

  (profiler/serve-files 8080)

  (def long-file-uri "file:///Users/jmaine/workspace/opensource/clojure-lsp/lib/src/clojure_lsp/feature/tmp.clj")
  (def short-range {:row 13
                    :col 1
                    :end-row 15
                    :end-col 1})
  (def med-range {:row 11
                  :col 1
                  :end-row 28
                  :end-col 2})
  (def long-range {:row 11
                   :col 1
                   :end-row 1028
                   :end-col 2})
  (def this-uri "file:///Users/jmaine/workspace/opensource/clojure-lsp/lib/src/clojure_lsp/feature/format.clj")
  (def uri long-file-uri)

  (formatting uri db/db)
  (formatting uri db/db)

  (time
    (let [pos {:row 145 :col 15 :end-row 152 :end-col 35}]
      #_(range-formatting this-uri pos db/db)
      #_(range-formatting-v1 this-uri pos db/db)
      (range-formatting-custom-to-pos this-uri pos db/db)))
  (time
    (let [pos {:row 0 :col 0 :end-row 1000 :end-col 35}]
      #_(range-formatting this-uri pos db/db)
      #_(range-formatting-v1 this-uri pos db/db)
      (range-formatting-custom-to-pos this-uri pos db/db)))

  (def orig-result (range-formatting-non-reduce-merge-format long-file-uri med-range db/db))
  (range-formatting-non-reduce-merge-format long-file-uri med-range db/db)
  (range-formatting-non-reduce-merge-format long-file-uri short-range db/db)
  (time (range-formatting-non-reduce-merge-format long-file-uri long-range db/db))
  (do
    (println "\n\n\n med v0")
    (bench/quick-bench
      (range-formatting long-file-uri med-range db/db)))
  (do
    (println "\n\n\n long v0")
    (bench/quick-bench
      (range-formatting long-file-uri long-range db/db)))
  (do
    (println "\n\n\n med find-forms map")
    (bench/quick-bench
      (range-formatting-non-reduce-map-reformat long-file-uri med-range db/db)))
  (do
    (println "\n\n\n long find-forms map")
    (bench/quick-bench
      (range-formatting-non-reduce-map-reformat long-file-uri long-range db/db)))
  (do
    (println "\n\n\n med find-forms merge")
    (bench/quick-bench
      (range-formatting-non-reduce-merge-format long-file-uri med-range db/db)))
  (do
    (println "\n\n\n long find-forms merge")
    (bench/quick-bench
      (range-formatting-non-reduce-merge-format long-file-uri long-range db/db)))
  (do
    (println "\n\n\n med to-pos")
    (bench/quick-bench
      (range-formatting-custom-to-pos long-file-uri med-range db/db)))
  (do
    (println "\n\n\n long to-pos")
    (bench/quick-bench
      (range-formatting-custom-to-pos long-file-uri long-range db/db)))

  (do
    (println "\n\n\n med custom to-pos")
    (bench/quick-bench
      (range-formatting-custom-to-pos long-file-uri med-range db/db)))
  (do
    (println "\n\n\n long custom to-pos")
    (bench/quick-bench
      (range-formatting-custom-to-pos long-file-uri long-range db/db)))
  (do
    (println "\n\n\n med v0")
    (time
      (profiler/profile
        {:min-width 5}
        (dotimes [_ 100]
          (range-formatting long-file-uri med-range db/db)))))
  (do
    (println "\n\n\n long v0")
    (time
      (profiler/profile
        {:min-width 5}
        (dotimes [_ 20]
          (range-formatting long-file-uri long-range db/db)))))
  (do
    (println "\n\n\n med find-forms map")
    (time
      (profiler/profile
        {:min-width 5}
        (dotimes [_ 100]
          (range-formatting-non-reduce-map-reformat long-file-uri med-range db/db)))))
  (do
    (println "\n\n\n long find-forms map")
    (time
      (profiler/profile
        {:min-width 5}
        (dotimes [_ 20]
          (range-formatting-non-reduce-map-reformat long-file-uri long-range db/db)))))
  (do
    (println "\n\n\n med find-forms merge")
    (time
      (profiler/profile
        {:min-width 5}
        (dotimes [_ 100]
          (range-formatting-non-reduce-merge-format long-file-uri med-range db/db)))))
  (do
    (println "\n\n\n long find-forms merge")
    (time
      (profiler/profile
        {:min-width 5}
        (dotimes [_ 40]
          (range-formatting-non-reduce-merge-format long-file-uri long-range db/db)))))
  (do
    (println "\n\n\n med to-loc")
    (time
      (profiler/profile
        {:min-width 5}
        (dotimes [_ 200]
          (range-formatting-custom-to-pos long-file-uri med-range db/db)))))
  (do
    (println "\n\n\n long to-loc")
    (time
      (profiler/profile
        {:min-width 5}
        (dotimes [_ 40]
          (range-formatting-custom-to-pos long-file-uri long-range db/db)))))

  (range-formatting-custom-to-pos long-file-uri long-range db/db))

