(ns clojure-lsp.feature.completion-snippet
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn known-snippets [function-call? settings]
  [;; Documentation
   {:label "comment"
    :detail "Insert comment block"
    :function-call function-call?
    :insert-text "(comment\n  $0\n  )"}
   {:label "comment-heading"
    :detail "Insert comment header"
    :insert-text
    ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; ${1:Namespace summary title}
    ;;
    ;; ${2:Brief description}\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n$0"}
   {:label "comment-separator"
    :detail "Insert comment separator"
    :insert-text
    ";; ${1:Namespace summary title}\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n$0"}
   {:label "rich-comment"
    :detail "Insert rich comment block"
    :function-call function-call?
    :insert-text
    "(comment
      $0
  #_())"}
   {:label "rich-comment-rdd"
    :detail "Insert rich comment rdd block"
    :insert-text
    "#_{:clj-kondo/ignore [:redefined-var]}
   (comment
     $0
   #_())"}
   {:label "rich-comment-hotload"
    :detail "Insert rich comment library hotload"
    :insert-text
    "#_{:clj-kondo/ignore [:redefined-var]}
    (comment
      ;; Add-lib library for hot-loading
      (require '[clojure.tools.deps.alpha.repl :refer [add-libs]])
      (add-libs '{${1:domain/library-name} {:mvn/version \"${2:1.0.0}\"}$3})
      $0
    )"}

   ;; Core
   {:label "condp"
    :detail "Insert condp"
    :function-call function-call?
    :insert-text "(condp ${1:pred} ${2:expr}\n $0)"}
   {:label "def"
    :function-call function-call?
    :insert-text "(def ${1:name} $0)"
    :detail "Insert def"}
   {:label "def-"
    :function-call function-call?
    :detail "Insert def private"
    :insert-text "(def ^:private ${1:name} $0)"}
   {:label "def-doc"
    :function-call function-call?
    :insert-text "(def ${1:name}\n  \"${2:docstring}\"\n  $0)"
    :detail "Insert def with docstring"}
   {:label "defmethod"
    :function-call function-call?
    :detail "Insert defmethod"
    :insert-text "(defmethod ${1:name} ${2:match}\n [${3:args}]\n $0)"}
   {:label "defmulti"
    :function-call function-call?
    :detail "Insert defmulti"
    :insert-text "(defmulti ${1:name} ${2:dispatch-fn})"}
   {:label "defn"
    :function-call function-call?
    :insert-text "(defn ${1:name} [$2]\n  $0)"
    :detail "Insert public defn"}
   {:label "defn-doc"
    :function-call function-call?
    :detail "Insert public defn with docstring"
    :insert-text "(defn ${1:name}\n  \"${2:docstring}\"\n   [${3:args}]\n  $0)"}
   {:label "defn-"
    :function-call function-call?
    :detail "Insert private defn"
    :insert-text (format "(defn%s ${1:name} [$2]\n  $0)"
                         (if (:use-metadata-for-privacy? settings)
                           " ^:private"
                           "-"))}
   {:label "defprotocol"
    :function-call function-call?
    :detail "Insert defprotocol"
    :insert-text "(defprotocol ${1:Name}\n $0)"}
   {:label "defrecord"
    :function-call function-call?
    :detail "Insert defrecord"
    :insert-text "(defrecord ${1:Name} [${2:fields}]\n ${3:Protocol}\n $0)"}
   {:label "deftype"
    :function-call function-call?
    :detail "Insert deftype"
    :insert-text "(deftype ${1:Name} [${2:fields}]\n ${3:Protocol}\n $0)"}
   {:label "fn"
    :function-call function-call?
    :detail "Insert fn"
    :insert-text "(fn [${1:arg-list}] $0)"}
   {:label "for"
    :function-call function-call?
    :detail "Insert for"
    :insert-text "(for [${1:item} ${2:coll}]\n  $0)"}
   {:label "if"
    :function-call function-call?
    :detail "Insert if"
    :insert-text "(if ${1:test-expr}\n ${2:then-expr}\n ${3:else-expr})"}
   {:label "kwargs"
    :detail "Insert keyword args"
    :insert-text "& {:keys [${1:keys}] :or {${2:defaults}}}"}
   {:label "let"
    :function-call function-call?
    :detail "Insert let"
    :insert-text "(let [${1:binding} ${2:value}])"}
   {:label "letfn"
    :function-call function-call?
    :detail "Insert letfn"
    :insert-text "(letfn [(${1:name} [${2:args}]\n $0)])"}
   {:label "ns"
    :function-call function-call?
    :detail "Insert ns"
    :insert-text "(ns ${1:name}\n  ${0:references})"}
   {:label "ns-doc"
    :function-call function-call?
    :detail "Insert ns with docstring"
    :insert-text "(ns ${1:name}\n  \"${2:docstring}\"\n  $0)"}

   ;; Requiring deps
   {:label "require"
    :detail "Insert ns :require"
    :insert-text "(:require [${1:namespace}])$0"}
   {:label "require-as"
    :detail "Insert ns require with :as alias"
    :insert-text "(:require [${1:namespace} :as ${2:alias}]$3)$0"}
   {:label "require-refer"
    :detail "Insert ns :require with :refer"
    :insert-text "(:require [${1:namespace} :refer [$2]]$3)$0"}
   {:label "require-rdd"
    :detail "Insert require for rich comment experiments"
    :insert-text "(require '[${1:namespace} :as ${2:alias}]$3)$0"}
   {:label "req-as"
    :detail "Insert single require dep :as alias"
    :insert-text "[${1:namespace} :as ${2:alias}]"}
   {:label "req-refer"
    :detail "Insert single require dep with :refer"
    :insert-text "[${1:namespace} :refer [$2]]"}
   {:label "import"
    :detail "Insert import"
    :insert-text "(:import [${1:package}])"}
   {:name "use"
    :detail "Insert require refer preferred over use"
    :snippet "(:require [${1:namespace} :refer [$2]])$0"}

   ;; deps.edn
   {:label "deps-alias"
    :detail "Insert alias with extra path & deps"
    :insert-text
    ":${1:category/name}
    {:extra-paths [\"${2:path}\"]
     :extra-deps {${3:deps-maven or deps-git}}}$0"}
   {:label "deps-maven"
    :detail "Insert maven dependency"
    :insert-text
    "${1:domain/library-name} {:mvn/version \"${2:1.0.0}\"}$0"}
   {:label "deps-git"
    :detail "Insert git dependency"
    :insert-text
    "${1:domain/library-name}
       {:git/sha \"${2:git-sha-value}\"}$0"}
   {:label "deps-git-tag"
    :detail "Insert git tag dependency"
    :insert-text
    "${1:domain/library-name}
      {:git/tag \"${2:git-tag-value}\"
       :git/sha \"${3:git-sha-value}\"}$0"}
   {:label "deps-git-url"
    :detail "Insert git URL dependency"
    :insert-text
    "${1:domain/library-name}
      {:git/url \"https://github.com/$1\"
       :git/sha \"${2:git-sha-value}\"}$0"}
   {:label "deps-local"
    :detail "Insert local dependency"
    :insert-text
    "${1:domain/library-name} {:local/root \"${2:/path/to/project/root}\"}$0"}

   ;; Testing
   {:label "deftest"
    :detail "Insert deftest clojure.test"
    :insert-text
    "(deftest ${1:name}-test
      (testing \"${2:Context of the test assertions}\"
        (is (= ${3:assertion-values}))$4)) $0"}
   {:label "testing"
    :detail "Insert testing clojure.test"
    :insert-text
    "(testing \"${1:Context of the test assertions}\"
       $0)"}
   {:label "is"
    :detail "Insert is clojure.test"
    :insert-text
    "(is (= ${1:assertion-values}))"}])

(defn ^:private replacing-current-form-vars [snippet next-loc]
  (let [current-sexpr (or (some-> next-loc z/string)
                          "")]
    (string/replace snippet "$current-form" current-sexpr)))

(defn build-additional-snippets [cursor-loc next-loc settings]
  (if-let [range-zloc (or next-loc cursor-loc)]
    (->> (get settings :additional-snippets [])
         (filter #(or (not (string/includes? (:snippet %) "$current-form"))
                      (and cursor-loc
                           next-loc
                           (meta (z/node range-zloc)))))
         (map (fn [{:keys [name detail snippet]}]
                (if (string/includes? snippet "$current-form")
                  (let [range (shared/->range (meta (z/node next-loc)))]
                    {:label name
                     :detail detail
                     :text-edit {:range (if (= :token (z/tag cursor-loc))
                                          (update-in range [:start :character] - (count (z/string cursor-loc)))
                                          range)
                                 :new-text (replacing-current-form-vars snippet next-loc)}})
                  {:label name
                   :detail detail
                   :insert-text (replacing-current-form-vars snippet range-zloc)}))))
    []))
