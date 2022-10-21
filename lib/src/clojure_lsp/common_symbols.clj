(ns clojure-lsp.common-symbols)

(set! *warn-on-reflection* true)

(def java-util-syms
  '#{Collection,
     Comparator,
     Deque,
     Enumeration,
     EventListener,
     Formattable,
     Iterator,
     List,
     ListIterator,
     Map,
     Map.Entry,
     NavigableMap,
     NavigableSet,
     Observer,
     PrimitiveIterator,
     PrimitiveIterator.OfDouble,
     PrimitiveIterator.OfInt,
     PrimitiveIterator.OfLong,
     Queue,
     RandomAccess,
     Set,
     SortedMap,
     SortedSet,
     Spliterator,
     Spliterator.OfDouble,
     Spliterator.OfInt,
     Spliterator.OfLong,
     Spliterator.OfPrimitive,
     AbstractCollection,
     AbstractList,
     AbstractMap,
     AbstractMap.SimpleEntry,
     AbstractMap.SimpleImmutableEntry,
     AbstractQueue,
     AbstractSequentialList,
     AbstractSet,
     ArrayDeque,
     ArrayList,
     Arrays,
     Base64,
     Base64.Decoder,
     Base64.Encoder,
     BitSet,
     Calendar,
     Calendar.Builder,
     Collections,
     Currency,
     Date,
     Dictionary,
     DoubleSummaryStatistics,
     EnumMap,
     EnumSet,
     EventListenerProxy,
     EventObject,
     FormattableFlags,
     Formatter,
     GregorianCalendar,
     HashMap,
     HashSet,
     Hashtable,
     IdentityHashMap,
     IntSummaryStatistics,
     LinkedHashMap,
     LinkedHashSet,
     LinkedList,
     ListResourceBundle,
     Locale,
     Locale.Builder,
     Locale.LanguageRange,
     LongSummaryStatistics,
     Objects,
     Observable,
     Optional,
     OptionalDouble,
     OptionalInt,
     OptionalLong,
     PriorityQueue,
     Properties,
     PropertyPermission,
     PropertyResourceBundle,
     Random,
     ResourceBundle,
     ResourceBundle.Control,
     Scanner,
     ServiceLoader,
     SimpleTimeZone,
     Spliterators,
     Spliterators.AbstractDoubleSpliterator,
     Spliterators.AbstractIntSpliterator,
     Spliterators.AbstractLongSpliterator,
     Spliterators.AbstractSpliterator,
     SplittableRandom,
     Stack,
     StringJoiner,
     StringTokenizer,
     Timer,
     TimerTask,
     TimeZone,
     TreeMap,
     TreeSet,
     UUID,
     Vector,
     WeakHashMap,
     Formatter.BigDecimalLayoutForm,
     Locale.Category,
     Locale.FilteringMode,
     ConcurrentModificationException,
     DuplicateFormatFlagsException,
     EmptyStackException,
     FormatFlagsConversionMismatchException,
     FormatterClosedException,
     IllegalFormatCodePointException,
     IllegalFormatConversionException,
     IllegalFormatException,
     IllegalFormatFlagsException,
     IllegalFormatPrecisionException,
     IllegalFormatWidthException,
     IllformedLocaleException,
     InputMismatchException,
     InvalidPropertiesFormatException,
     MissingFormatArgumentException,
     MissingFormatWidthException,
     MissingResourceException,
     NoSuchElementException,
     TooManyListenersException,
     UnknownFormatConversionException,
     UnknownFormatFlagsException,
     ServiceConfigurationError})

(def clj-syms
  '#{{:name * :kind :function}
     {:name *' :kind :function}
     {:name *1 :kind :variable}
     {:name *2 :kind :variable}
     {:name *3 :kind :variable}
     {:name *agent* :kind :reference}
     {:name *allow-unresolved-vars* :kind :reference}
     {:name *assert* :kind :reference}
     {:name *clojure-version* :kind :reference}
     {:name *command-line-args* :kind :reference}
     {:name *compile-files* :kind :reference}
     {:name *compile-path* :kind :reference}
     {:name *compiler-options* :kind :reference}
     {:name *data-readers* :kind :reference}
     {:name *default-data-reader-fn* :kind :reference}
     {:name *e :kind :variable}
     {:name *err* :kind :reference}
     {:name *file* :kind :reference}
     {:name *flush-on-newline* :kind :reference}
     {:name *fn-loader* :kind :reference}
     {:name *in* :kind :reference}
     {:name *math-context* :kind :reference}
     {:name *ns* :kind :reference}
     {:name *out* :kind :reference}
     {:name *print-dup* :kind :reference}
     {:name *print-length* :kind :reference}
     {:name *print-level* :kind :reference}
     {:name *print-meta* :kind :reference}
     {:name *print-namespace-maps* :kind :reference}
     {:name *print-readably* :kind :reference}
     {:name *read-eval* :kind :reference}
     {:name *reader-resolver* :kind :reference}
     {:name *source-path* :kind :reference}
     {:name *suppress-read* :kind :reference}
     {:name *unchecked-math* :kind :reference}
     {:name *use-context-classloader* :kind :reference}
     {:name *verbose-defrecords* :kind :reference}
     {:name *warn-on-reflection* :kind :reference}
     {:name + :kind :function}
     {:name +' :kind :function}
     {:name - :kind :function}
     {:name -' :kind :function}
     {:name -> :kind :function}
     {:name ->> :kind :function}
     {:name ->ArrayChunk :kind :reference}
     {:name ArrayChunk :kind :variable}
     {:name ArrayManager, :kind :variable}
     {:name ->Eduction :kind :reference}
     {:name Eduction :kind :variable}
     {:name IVecImpl, :kind :variable}
     {:name NaN?, :kind :function}
     {:name PrintWriter-on, :kind :function}
     {:name ->Vec :kind :reference}
     {:name Vec, :kind :variable}
     {:name ->VecNode :kind :reference}
     {:name VecNode, :kind :variable}
     {:name ->VecSeq :kind :reference}
     {:name VecSeq, :kind :variable}
     {:name -cache-protocol-fn :kind :function}
     {:name -reset-methods :kind :function}
     {:name . :kind :function}
     {:name .. :kind :function}
     {:name / :kind :function}
     {:name < :kind :function}
     {:name <= :kind :function}
     {:name = :kind :function}
     {:name == :kind :function}
     {:name > :kind :function}
     {:name >= :kind :function}
     {:name abs, :kind :function}
     {:name accessor :kind :function}
     {:name aclone :kind :function}
     {:name add-classpath :kind :function}
     {:name add-tap :kind :function}
     {:name add-watch :kind :function}
     {:name agent :kind :function}
     {:name agent-error :kind :function}
     {:name agent-errors :kind :function}
     {:name aget :kind :function}
     {:name alength :kind :function}
     {:name alias :kind :function}
     {:name all-ns :kind :function}
     {:name alter :kind :function}
     {:name alter-meta! :kind :function}
     {:name alter-var-root :kind :function}
     {:name amap :kind :function}
     {:name ancestors :kind :function}
     {:name and :kind :function}
     {:name any? :kind :function}
     {:name apply :kind :function}
     {:name areduce :kind :function}
     {:name array-map :kind :function}
     {:name as-> :kind :function}
     {:name aset :kind :function}
     {:name aset-boolean :kind :function}
     {:name aset-byte :kind :function}
     {:name aset-char :kind :function}
     {:name aset-double :kind :function}
     {:name aset-float :kind :function}
     {:name aset-int :kind :function}
     {:name aset-long :kind :function}
     {:name aset-short :kind :function}
     {:name assert :kind :function}
     {:name assoc :kind :function}
     {:name assoc! :kind :function}
     {:name assoc-in :kind :function}
     {:name associative? :kind :function}
     {:name atom :kind :function}
     {:name await :kind :function}
     {:name await-for :kind :function}
     {:name await1 :kind :function}
     {:name bases :kind :function}
     {:name bean :kind :function}
     {:name bigdec :kind :function}
     {:name bigint :kind :function}
     {:name biginteger :kind :function}
     {:name binding :kind :function}
     {:name bit-and :kind :function}
     {:name bit-and-not :kind :function}
     {:name bit-clear :kind :function}
     {:name bit-flip :kind :function}
     {:name bit-not :kind :function}
     {:name bit-or :kind :function}
     {:name bit-set :kind :function}
     {:name bit-shift-left :kind :function}
     {:name bit-shift-right :kind :function}
     {:name bit-test :kind :function}
     {:name bit-xor :kind :function}
     {:name boolean :kind :function}
     {:name boolean-array :kind :function}
     {:name boolean? :kind :function}
     {:name booleans :kind :function}
     {:name bound-fn :kind :function}
     {:name bound-fn* :kind :function}
     {:name bound? :kind :function}
     {:name bounded-count :kind :function}
     {:name butlast :kind :function}
     {:name byte :kind :function}
     {:name byte-array :kind :function}
     {:name bytes :kind :function}
     {:name bytes? :kind :function}
     {:name case :kind :function}
     {:name cast :kind :function}
     {:name cat :kind :function}
     {:name catch :kind :function}
     {:name char :kind :function}
     {:name char-array :kind :function}
     {:name char-escape-string :kind :function}
     {:name char-name-string :kind :function}
     {:name char? :kind :function}
     {:name chars :kind :function}
     {:name chunk :kind :function}
     {:name chunk-append :kind :function}
     {:name chunk-buffer :kind :function}
     {:name chunk-cons :kind :function}
     {:name chunk-first :kind :function}
     {:name chunk-next :kind :function}
     {:name chunk-rest :kind :function}
     {:name chunked-seq? :kind :function}
     {:name class :kind :function}
     {:name class? :kind :function}
     {:name clear-agent-errors :kind :function}
     {:name clojure-version :kind :function}
     {:name coll? :kind :function}
     {:name comment :kind :function}
     {:name commute :kind :function}
     {:name comp :kind :function}
     {:name comparator :kind :function}
     {:name compare :kind :function}
     {:name compare-and-set! :kind :function}
     {:name compile :kind :function}
     {:name complement :kind :function}
     {:name completing :kind :function}
     {:name concat :kind :function}
     {:name cond :kind :function}
     {:name cond-> :kind :function}
     {:name cond->> :kind :function}
     {:name condp :kind :function}
     {:name conj :kind :function}
     {:name conj! :kind :function}
     {:name cons :kind :function}
     {:name constantly :kind :function}
     {:name construct-proxy :kind :function}
     {:name contains? :kind :function}
     {:name count :kind :function}
     {:name counted? :kind :function}
     {:name create-ns :kind :function}
     {:name create-struct :kind :function}
     {:name cycle :kind :function}
     {:name dec :kind :function}
     {:name dec' :kind :function}
     {:name decimal? :kind :function}
     {:name declare :kind :function}
     {:name dedupe :kind :function}
     {:name def :kind :reference}
     {:name default-data-readers :kind :variable}
     {:name definline :kind :function}
     {:name definterface :kind :function}
     {:name defmacro :kind :function}
     {:name defmethod :kind :function}
     {:name defmulti :kind :function}
     {:name defn :kind :function}
     {:name defn- :kind :function}
     {:name defonce :kind :function}
     {:name defprotocol :kind :function}
     {:name defrecord :kind :function}
     {:name defstruct :kind :function}
     {:name deftype :kind :function}
     {:name delay :kind :function}
     {:name delay? :kind :function}
     {:name deliver :kind :function}
     {:name denominator :kind :function}
     {:name deref :kind :function}
     {:name derive :kind :function}
     {:name descendants :kind :function}
     {:name destructure :kind :function}
     {:name disj :kind :function}
     {:name disj! :kind :function}
     {:name dissoc :kind :function}
     {:name dissoc! :kind :function}
     {:name distinct :kind :function}
     {:name distinct? :kind :function}
     {:name do :kind :reference}
     {:name doall :kind :function}
     {:name dorun :kind :function}
     {:name doseq :kind :function}
     {:name dosync :kind :function}
     {:name dotimes :kind :function}
     {:name doto :kind :function}
     {:name double :kind :function}
     {:name double-array :kind :function}
     {:name double? :kind :function}
     {:name doubles :kind :function}
     {:name drop :kind :function}
     {:name drop-last :kind :function}
     {:name drop-while :kind :function}
     {:name eduction :kind :function}
     {:name empty :kind :function}
     {:name EMPTY-NODE :kind :variable}
     {:name empty? :kind :function}
     {:name ensure :kind :function}
     {:name ensure-reduced :kind :function}
     {:name enumeration-seq :kind :function}
     {:name error-handler :kind :function}
     {:name error-mode :kind :function}
     {:name eval :kind :function}
     {:name even? :kind :function}
     {:name every-pred :kind :function}
     {:name every? :kind :function}
     {:name ex-cause :kind :function}
     {:name ex-data :kind :function}
     {:name ex-info :kind :function}
     {:name ex-message :kind :function}
     {:name extend :kind :function}
     {:name extend-protocol :kind :function}
     {:name extend-type :kind :function}
     {:name extenders :kind :function}
     {:name extends? :kind :function}
     {:name false? :kind :function}
     {:name ffirst :kind :function}
     {:name file-seq :kind :function}
     {:name filter :kind :function}
     {:name filterv :kind :function}
     {:name finally :kind :reference}
     {:name find :kind :function}
     {:name find-keyword :kind :function}
     {:name find-ns :kind :function}
     {:name find-protocol-impl :kind :function}
     {:name find-protocol-method :kind :function}
     {:name find-var :kind :function}
     {:name first :kind :function}
     {:name flatten :kind :function}
     {:name float :kind :function}
     {:name float-array :kind :function}
     {:name float? :kind :function}
     {:name floats :kind :function}
     {:name flush :kind :function}
     {:name fn :kind :function}
     {:name fn? :kind :function}
     {:name fnext :kind :function}
     {:name fnil :kind :function}
     {:name for :kind :function}
     {:name force :kind :function}
     {:name format :kind :function}
     {:name frequencies :kind :function}
     {:name future :kind :function}
     {:name future-call :kind :function}
     {:name future-cancel :kind :function}
     {:name future-cancelled? :kind :function}
     {:name future-done? :kind :function}
     {:name future? :kind :function}
     {:name gen-and-load-class, :kind :function}
     {:name gen-class :kind :function}
     {:name gen-interface :kind :function}
     {:name gensym :kind :function}
     {:name get :kind :function}
     {:name get-in :kind :function}
     {:name get-method :kind :function}
     {:name get-proxy-class :kind :function}
     {:name get-thread-bindings :kind :function}
     {:name get-validator :kind :function}
     {:name global-hierarchy, :kind :variable}
     {:name group-by :kind :function}
     {:name halt-when :kind :function}
     {:name hash :kind :function}
     {:name hash-combine :kind :function}
     {:name hash-map :kind :function}
     {:name hash-ordered-coll :kind :function}
     {:name hash-set :kind :function}
     {:name hash-unordered-coll :kind :function}
     {:name ident? :kind :function}
     {:name identical? :kind :function}
     {:name identity :kind :function}
     {:name if :kind :reference}
     {:name if-let :kind :function}
     {:name if-not :kind :function}
     {:name if-some :kind :function}
     {:name ifn? :kind :function}
     {:name import :kind :function}
     {:name in-ns :kind :reference}
     {:name inc :kind :function}
     {:name inc' :kind :function}
     {:name indexed? :kind :function}
     {:name infinite? :kind :function}
     {:name init-proxy :kind :function}
     {:name Inst :kind :variable}
     {:name inst-ms :kind :function}
     {:name inst-ms* :kind :function}
     {:name inst? :kind :function}
     {:name instance? :kind :function}
     {:name int :kind :function}
     {:name int-array :kind :function}
     {:name int? :kind :function}
     {:name integer? :kind :function}
     {:name interleave :kind :function}
     {:name intern :kind :function}
     {:name interpose :kind :function}
     {:name into :kind :function}
     {:name into-array :kind :function}
     {:name ints :kind :function}
     {:name io! :kind :function}
     {:name isa? :kind :function}
     {:name iterate :kind :function}
     {:name iteration, :kind :function}
     {:name iterator-seq :kind :function}
     {:name juxt :kind :function}
     {:name keep :kind :function}
     {:name keep-indexed :kind :function}
     {:name key :kind :function}
     {:name keys :kind :function}
     {:name keyword :kind :function}
     {:name keyword? :kind :function}
     {:name last :kind :function}
     {:name lazy-cat :kind :function}
     {:name lazy-seq :kind :function}
     {:name let :kind :function}
     {:name letfn :kind :function}
     {:name line-seq :kind :function}
     {:name list :kind :function}
     {:name list* :kind :function}
     {:name list? :kind :function}
     {:name load :kind :function}
     {:name load-file :kind :reference}
     {:name load-reader :kind :function}
     {:name load-string :kind :function}
     {:name loaded-libs :kind :function}
     {:name locking :kind :function}
     {:name long :kind :function}
     {:name long-array :kind :function}
     {:name longs :kind :function}
     {:name loop :kind :function}
     {:name macroexpand :kind :function}
     {:name macroexpand-1 :kind :function}
     {:name make-array :kind :function}
     {:name make-hierarchy :kind :function}
     {:name map :kind :function}
     {:name map-entry? :kind :function}
     {:name map-indexed :kind :function}
     {:name map? :kind :function}
     {:name mapcat :kind :function}
     {:name mapv :kind :function}
     {:name max :kind :function}
     {:name max-key :kind :function}
     {:name memfn :kind :function}
     {:name memoize :kind :function}
     {:name merge :kind :function}
     {:name merge-with :kind :function}
     {:name meta :kind :function}
     {:name method-sig :kind :function}
     {:name methods :kind :function}
     {:name min :kind :function}
     {:name min-key :kind :function}
     {:name mix-collection-hash :kind :function}
     {:name mod :kind :function}
     {:name monitor-enter :kind :reference}
     {:name monitor-exit :kind :reference}
     {:name munge :kind :function}
     {:name name :kind :function}
     {:name namespace :kind :function}
     {:name namespace-munge :kind :function}
     {:name nat-int? :kind :function}
     {:name neg-int? :kind :function}
     {:name neg? :kind :function}
     {:name new :kind :reference}
     {:name newline :kind :function}
     {:name next :kind :function}
     {:name nfirst :kind :function}
     {:name nil? :kind :function}
     {:name nnext :kind :function}
     {:name not :kind :function}
     {:name not-any? :kind :function}
     {:name not-empty :kind :function}
     {:name not-every? :kind :function}
     {:name not= :kind :function}
     {:name ns :kind :function}
     {:name ns-aliases :kind :function}
     {:name ns-imports :kind :function}
     {:name ns-interns :kind :function}
     {:name ns-map :kind :function}
     {:name ns-name :kind :function}
     {:name ns-publics :kind :function}
     {:name ns-refers :kind :function}
     {:name ns-resolve :kind :function}
     {:name ns-unalias :kind :function}
     {:name ns-unmap :kind :function}
     {:name nth :kind :function}
     {:name nthnext :kind :function}
     {:name nthrest :kind :function}
     {:name num :kind :function}
     {:name number? :kind :function}
     {:name numerator :kind :function}
     {:name object-array :kind :function}
     {:name odd? :kind :function}
     {:name or :kind :function}
     {:name parents :kind :function}
     {:name parse-boolean, :kind :function}
     {:name parse-double, :kind :function}
     {:name parse-long, :kind :function}
     {:name parse-uuid, :kind :function}
     {:name partial :kind :function}
     {:name partition :kind :function}
     {:name partition-all :kind :function}
     {:name partition-by :kind :function}
     {:name pcalls :kind :function}
     {:name peek :kind :function}
     {:name persistent! :kind :function}
     {:name pmap :kind :function}
     {:name pop :kind :function}
     {:name pop! :kind :function}
     {:name pop-thread-bindings :kind :function}
     {:name pos-int? :kind :function}
     {:name pos? :kind :function}
     {:name pr :kind :function}
     {:name pr-str :kind :function}
     {:name prefer-method :kind :function}
     {:name prefers :kind :function}
     {:name primitives-classnames :kind :variable}
     {:name print :kind :function}
     {:name print-ctor :kind :function}
     {:name print-dup :kind :function}
     {:name print-method :kind :function}
     {:name print-simple :kind :function}
     {:name print-str :kind :function}
     {:name printf :kind :function}
     {:name println :kind :function}
     {:name println-str :kind :function}
     {:name prn :kind :function}
     {:name prn-str :kind :function}
     {:name process-annotation, :kind :variable}
     {:name promise :kind :function}
     {:name proxy :kind :function}
     {:name proxy-call-with-super :kind :function}
     {:name proxy-mappings :kind :function}
     {:name proxy-name :kind :function}
     {:name proxy-super :kind :function}
     {:name push-thread-bindings :kind :function}
     {:name pvalues :kind :function}
     {:name qualified-ident? :kind :function}
     {:name qualified-keyword? :kind :function}
     {:name qualified-symbol? :kind :function}
     {:name quot :kind :function}
     {:name quote :kind :reference}
     {:name rand :kind :function}
     {:name rand-int :kind :function}
     {:name rand-nth :kind :function}
     {:name random-sample :kind :function}
     {:name random-uuid :kind :function}
     {:name range :kind :function}
     {:name ratio? :kind :function}
     {:name rational? :kind :function}
     {:name rationalize :kind :function}
     {:name re-find :kind :function}
     {:name re-groups :kind :function}
     {:name re-matcher :kind :function}
     {:name re-matches :kind :function}
     {:name re-pattern :kind :function}
     {:name re-seq :kind :function}
     {:name read :kind :function}
     {:name read-line :kind :function}
     {:name read+string, :kind :function}
     {:name read-string :kind :function}
     {:name reader-conditional :kind :function}
     {:name reader-conditional? :kind :function}
     {:name realized? :kind :function}
     {:name record? :kind :function}
     {:name recur :kind :reference}
     {:name reduce :kind :function}
     {:name reduce-kv :kind :function}
     {:name reduced :kind :function}
     {:name reduced? :kind :function}
     {:name reductions :kind :function}
     {:name ref :kind :function}
     {:name ref-history-count :kind :function}
     {:name ref-max-history :kind :function}
     {:name ref-min-history :kind :function}
     {:name ref-set :kind :function}
     {:name refer :kind :function}
     {:name refer-clojure :kind :function}
     {:name reify :kind :function}
     {:name release-pending-sends :kind :function}
     {:name rem :kind :function}
     {:name remove :kind :function}
     {:name remove-all-methods :kind :function}
     {:name remove-method :kind :function}
     {:name remove-ns :kind :function}
     {:name remove-tap :kind :function}
     {:name remove-watch :kind :function}
     {:name repeat :kind :function}
     {:name repeatedly :kind :function}
     {:name replace :kind :function}
     {:name replicate :kind :function}
     {:name require :kind :function}
     {:name requiring-resolve, :kind :function}
     {:name reset! :kind :function}
     {:name reset-meta! :kind :function}
     {:name reset-vals! :kind :function}
     {:name resolve :kind :function}
     {:name rest :kind :function}
     {:name restart-agent :kind :function}
     {:name resultset-seq :kind :function}
     {:name reverse :kind :function}
     {:name reversible? :kind :function}
     {:name rseq :kind :function}
     {:name rsubseq :kind :function}
     {:name run! :kind :function}
     {:name satisfies? :kind :function}
     {:name second :kind :function}
     {:name select-keys :kind :function}
     {:name send :kind :function}
     {:name send-off :kind :function}
     {:name send-via :kind :function}
     {:name seq :kind :function}
     {:name seq-to-map-for-destructuring, :kind :function}
     {:name seq? :kind :function}
     {:name seqable? :kind :function}
     {:name seque :kind :function}
     {:name sequence :kind :function}
     {:name sequential? :kind :function}
     {:name set :kind :function}
     {:name set! :kind :reference}
     {:name set-agent-send-executor! :kind :function}
     {:name set-agent-send-off-executor! :kind :function}
     {:name set-error-handler! :kind :function}
     {:name set-error-mode! :kind :function}
     {:name set-validator! :kind :function}
     {:name set? :kind :function}
     {:name short :kind :function}
     {:name short-array :kind :function}
     {:name shorts :kind :function}
     {:name shuffle :kind :function}
     {:name shutdown-agents :kind :function}
     {:name simple-ident? :kind :function}
     {:name simple-keyword? :kind :function}
     {:name simple-symbol? :kind :function}
     {:name slurp :kind :function}
     {:name some :kind :function}
     {:name some-> :kind :function}
     {:name some->> :kind :function}
     {:name some-fn :kind :function}
     {:name some? :kind :function}
     {:name sort :kind :function}
     {:name sort-by :kind :function}
     {:name sorted-map :kind :function}
     {:name sorted-map-by :kind :function}
     {:name sorted-set :kind :function}
     {:name sorted-set-by :kind :function}
     {:name sorted? :kind :function}
     {:name special-symbol? :kind :function}
     {:name spit :kind :function}
     {:name split-at :kind :function}
     {:name split-with :kind :function}
     {:name StackTraceElement->vec :kind :function}
     {:name str :kind :function}
     {:name string? :kind :function}
     {:name struct :kind :function}
     {:name struct-map :kind :function}
     {:name subs :kind :function}
     {:name subseq :kind :function}
     {:name subvec :kind :function}
     {:name supers :kind :function}
     {:name swap! :kind :function}
     {:name swap-vals! :kind :function}
     {:name symbol :kind :function}
     {:name symbol? :kind :function}
     {:name sync :kind :function}
     {:name tagged-literal :kind :function}
     {:name tagged-literal? :kind :function}
     {:name take :kind :function}
     {:name take-last :kind :function}
     {:name take-nth :kind :function}
     {:name take-while :kind :function}
     {:name tap> :kind :function}
     {:name test :kind :function}
     {:name the-ns :kind :function}
     {:name thread-bound? :kind :function}
     {:name throw :kind :reference}
     {:name Throwable->map :kind :function}
     {:name time :kind :function}
     {:name to-array :kind :function}
     {:name to-array-2d :kind :function}
     {:name trampoline :kind :function}
     {:name transduce :kind :function}
     {:name transient :kind :function}
     {:name tree-seq :kind :function}
     {:name true? :kind :function}
     {:name try :kind :reference}
     {:name type :kind :function}
     {:name unchecked-add :kind :function}
     {:name unchecked-add-int :kind :function}
     {:name unchecked-byte :kind :function}
     {:name unchecked-char :kind :function}
     {:name unchecked-dec :kind :function}
     {:name unchecked-dec-int :kind :function}
     {:name unchecked-divide-int :kind :function}
     {:name unchecked-double :kind :function}
     {:name unchecked-float :kind :function}
     {:name unchecked-inc :kind :function}
     {:name unchecked-inc-int :kind :function}
     {:name unchecked-int :kind :function}
     {:name unchecked-long :kind :function}
     {:name unchecked-multiply :kind :function}
     {:name unchecked-multiply-int :kind :function}
     {:name unchecked-negate :kind :function}
     {:name unchecked-negate-int :kind :function}
     {:name unchecked-remainder-int :kind :function}
     {:name unchecked-short :kind :function}
     {:name unchecked-subtract :kind :function}
     {:name unchecked-subtract-int :kind :function}
     {:name underive :kind :function}
     {:name unquote :kind :variable}
     {:name unquote-splicing :kind :variable}
     {:name unreduced :kind :function}
     {:name unsigned-bit-shift-right :kind :function}
     {:name update :kind :function}
     {:name update-in :kind :function}
     {:name update-keys, :kind :function}
     {:name update-proxy :kind :function}
     {:name update-vals, :kind :function}
     {:name uri? :kind :function}
     {:name use :kind :function}
     {:name uuid? :kind :function}
     {:name val :kind :function}
     {:name vals :kind :function}
     {:name var :kind :reference}
     {:name var-get :kind :function}
     {:name var-set :kind :function}
     {:name var? :kind :function}
     {:name vary-meta :kind :function}
     {:name vec :kind :function}
     {:name vector :kind :function}
     {:name vector-of :kind :function}
     {:name vector? :kind :function}
     {:name volatile! :kind :function}
     {:name volatile? :kind :function}
     {:name vreset! :kind :function}
     {:name vswap! :kind :function}
     {:name when :kind :function}
     {:name when-first :kind :function}
     {:name when-let :kind :function}
     {:name when-not :kind :function}
     {:name when-some :kind :function}
     {:name while :kind :function}
     {:name with-bindings :kind :function}
     {:name with-bindings* :kind :function}
     {:name with-in-str :kind :function}
     {:name with-loading-context :kind :function}
     {:name with-local-vars :kind :function}
     {:name with-meta :kind :function}
     {:name with-open :kind :function}
     {:name with-out-str :kind :function}
     {:name with-precision :kind :function}
     {:name with-redefs :kind :function}
     {:name with-redefs-fn :kind :function}
     {:name xml-seq :kind :function}
     {:name zero? :kind :function}
     {:name zipmap :kind :function}})

(def cljs-syms
  '#{{:name *clojurescript-version* :kind :variable}
     {:name *eval* :kind :variable}
     {:name *exec-tap-fn* :kind :function}
     {:name *loaded-libs* :kind :variable}
     {:name *main-cli-fn* :kind :variable}
     {:name *print-err-fn* :kind :variable}
     {:name *print-fn* :kind :variable}
     {:name *print-fn-bodies* :kind :variable}
     {:name *print-newline* :kind :variable}
     {:name *target* :kind :variable}
     {:name *unchecked-arrays* :kind :variable}
     {:name *unchecked-if* :kind :variable}
     {:name *warn-on-infer* :kind :variable}
     {:name ->ArrayIter :kind :function}
     {:name ->ArrayList :kind :function}
     {:name ->ArrayNode :kind :function}
     {:name ->ArrayNodeIterator :kind :function}
     {:name ->ArrayNodeSeq :kind :function}
     {:name ->Atom :kind :function}
     {:name ->BitmapIndexedNode :kind :function}
     {:name ->BlackNode :kind :function}
     {:name ->Box :kind :function}
     {:name ->ChunkBuffer :kind :function}
     {:name ->ChunkedCons :kind :function}
     {:name ->ChunkedSeq :kind :function}
     {:name ->Cons :kind :function}
     {:name ->Cycle :kind :function}
     {:name ->Delay :kind :function}
     {:name ->ES6EntriesIterator :kind :function}
     {:name ->ES6Iterator :kind :function}
     {:name ->ES6IteratorSeq :kind :function}
     {:name ->ES6SetEntriesIterator :kind :function}
     {:name ->Empty :kind :function}
     {:name ->EmptyList :kind :function}
     {:name ->HashCollisionNode :kind :function}
     {:name ->HashMapIter :kind :function}
     {:name ->HashSetIter :kind :function}
     {:name ->IndexedSeq :kind :function}
     {:name ->IndexedSeqIterator :kind :function}
     {:name ->Iterate :kind :function}
     {:name ->KeySeq :kind :function}
     {:name ->Keyword :kind :function}
     {:name ->LazySeq :kind :function}
     {:name ->List :kind :function}
     {:name ->Many :kind :function}
     {:name ->MapEntry :kind :function}
     {:name ->MetaFn :kind :function}
     {:name ->MultiFn :kind :function}
     {:name ->MultiIterator :kind :function}
     {:name ->Namespace :kind :function}
     {:name ->NeverEquiv :kind :function}
     {:name ->NodeIterator :kind :function}
     {:name ->NodeSeq :kind :function}
     {:name ->ObjMap :kind :function}
     {:name ->PersistentArrayMap :kind :function}
     {:name ->PersistentArrayMapIterator :kind :function}
     {:name ->PersistentArrayMapSeq :kind :function}
     {:name ->PersistentHashMap :kind :function}
     {:name ->PersistentHashSet :kind :function}
     {:name ->PersistentQueue :kind :function}
     {:name ->PersistentQueueIter :kind :function}
     {:name ->PersistentQueueSeq :kind :function}
     {:name ->PersistentTreeMap :kind :function}
     {:name ->PersistentTreeMapSeq :kind :function}
     {:name ->PersistentTreeSet :kind :function}
     {:name ->PersistentVector :kind :function}
     {:name ->RSeq :kind :function}
     {:name ->Range :kind :function}
     {:name ->RangeChunk :kind :function}
     {:name ->RangeIterator :kind :function}
     {:name ->RangedIterator :kind :function}
     {:name ->RecordIter :kind :function}
     {:name ->RedNode :kind :function}
     {:name ->Reduced :kind :function}
     {:name ->Repeat :kind :function}
     {:name ->SeqIter :kind :function}
     {:name ->Single :kind :function}
     {:name ->StringBufferWriter :kind :function}
     {:name ->StringIter :kind :function}
     {:name ->Subvec :kind :function}
     {:name ->Symbol :kind :function}
     {:name ->TaggedLiteral :kind :function}
     {:name ->TransformerIterator :kind :function}
     {:name ->TransientArrayMap :kind :function}
     {:name ->TransientHashMap :kind :function}
     {:name ->TransientHashSet :kind :function}
     {:name ->TransientVector :kind :function}
     {:name ->UUID :kind :function}
     {:name ->ValSeq :kind :function}
     {:name ->Var :kind :function}
     {:name ->VectorNode :kind :function}
     {:name ->Volatile :kind :function}
     {:name -add-method :kind :function}
     {:name -add-watch :kind :function}
     {:name -as-transient :kind :function}
     {:name -assoc :kind :function}
     {:name -assoc! :kind :function}
     {:name -assoc-n :kind :function}
     {:name -assoc-n! :kind :function}
     {:name -chunked-first :kind :function}
     {:name -chunked-next :kind :function}
     {:name -chunked-rest :kind :function}
     {:name -clj->js :kind :function}
     {:name -clone :kind :function}
     {:name -comparator :kind :function}
     {:name -compare :kind :function}
     {:name -conj :kind :function}
     {:name -conj! :kind :function}
     {:name -contains-key? :kind :function}
     {:name -count :kind :function}
     {:name -default-dispatch-val :kind :function}
     {:name -deref :kind :function}
     {:name -deref-with-timeout :kind :function}
     {:name -disjoin :kind :function}
     {:name -disjoin! :kind :function}
     {:name -dispatch-fn :kind :function}
     {:name -dissoc :kind :function}
     {:name -dissoc! :kind :function}
     {:name -drop-first :kind :function}
     {:name -empty :kind :function}
     {:name -entry-key :kind :function}
     {:name -equiv :kind :function}
     {:name -find :kind :function}
     {:name -first :kind :function}
     {:name -flush :kind :function}
     {:name -get-method :kind :function}
     {:name -hash :kind :function}
     {:name -invoke :kind :function}
     {:name -iterator :kind :function}
     {:name -js->clj :kind :function}
     {:name -key :kind :function}
     {:name -key->js :kind :function}
     {:name -kv-reduce :kind :function}
     {:name -lookup :kind :function}
     {:name -meta :kind :function}
     {:name -methods :kind :function}
     {:name -name :kind :function}
     {:name -namespace :kind :function}
     {:name -next :kind :function}
     {:name -notify-watches :kind :function}
     {:name -nth :kind :function}
     {:name -peek :kind :function}
     {:name -persistent! :kind :function}
     {:name -pop :kind :function}
     {:name -pop! :kind :function}
     {:name -pr-writer :kind :function}
     {:name -prefer-method :kind :function}
     {:name -prefers :kind :function}
     {:name -realized? :kind :function}
     {:name -reduce :kind :function}
     {:name -remove-method :kind :function}
     {:name -remove-watch :kind :function}
     {:name -reset :kind :function}
     {:name -reset! :kind :function}
     {:name -rest :kind :function}
     {:name -rseq :kind :function}
     {:name -seq :kind :function}
     {:name -sorted-seq :kind :function}
     {:name -sorted-seq-from :kind :function}
     {:name -swap! :kind :function}
     {:name -val :kind :function}
     {:name -vreset! :kind :function}
     {:name -with-meta :kind :function}
     {:name -write :kind :function}
     {:name APersistentVector :kind :variable}
     {:name ASeq :kind :variable}
     {:name ArrayIter :kind :variable}
     {:name ArrayList :kind :variable}
     {:name ArrayNode :kind :variable}
     {:name ArrayNodeIterator :kind :variable}
     {:name ArrayNodeSeq :kind :variable}
     {:name Atom :kind :variable}
     {:name BitmapIndexedNode :kind :variable}
     {:name BlackNode :kind :variable}
     {:name Box :kind :variable}
     {:name CHAR_MAP :kind :variable}
     {:name ChunkBuffer :kind :variable}
     {:name ChunkedCons :kind :variable}
     {:name ChunkedSeq :kind :variable}
     {:name Cons :kind :variable}
     {:name Cycle :kind :variable}
     {:name DEMUNGE_MAP :kind :variable}
     {:name DEMUNGE_PATTERN :kind :variable}
     {:name Delay :kind :variable}
     {:name ES6EntriesIterator :kind :variable}
     {:name ES6Iterator :kind :variable}
     {:name ES6IteratorSeq :kind :variable}
     {:name ES6SetEntriesIterator :kind :variable}
     {:name Empty :kind :variable}
     {:name EmptyList :kind :variable}
     {:name ExceptionInfo :kind :function}
     {:name Fn :kind :variable}
     {:name HashCollisionNode :kind :variable}
     {:name HashMapIter :kind :variable}
     {:name HashSetIter :kind :variable}
     {:name IAssociative :kind :variable}
     {:name IAtom :kind :variable}
     {:name IChunk :kind :variable}
     {:name IChunkedNext :kind :variable}
     {:name IChunkedSeq :kind :variable}
     {:name ICloneable :kind :variable}
     {:name ICollection :kind :variable}
     {:name IComparable :kind :variable}
     {:name ICounted :kind :variable}
     {:name IDeref :kind :variable}
     {:name IDerefWithTimeout :kind :variable}
     {:name IEditableCollection :kind :variable}
     {:name IEmptyableCollection :kind :variable}
     {:name IEncodeClojure :kind :variable}
     {:name IEncodeJS :kind :variable}
     {:name IEquiv :kind :variable}
     {:name IFind :kind :variable}
     {:name IFn :kind :variable}
     {:name IHash :kind :variable}
     {:name IIndexed :kind :variable}
     {:name IIterable :kind :variable}
     {:name IKVReduce :kind :variable}
     {:name IList :kind :variable}
     {:name ILookup :kind :variable}
     {:name IMap :kind :variable}
     {:name IMapEntry :kind :variable}
     {:name IMeta :kind :variable}
     {:name IMultiFn :kind :variable}
     {:name INIT :kind :variable}
     {:name INamed :kind :variable}
     {:name INext :kind :variable}
     {:name IPending :kind :variable}
     {:name IPrintWithWriter :kind :variable}
     {:name IRecord :kind :variable}
     {:name IReduce :kind :variable}
     {:name IReset :kind :variable}
     {:name IReversible :kind :variable}
     {:name ISeq :kind :variable}
     {:name ISeqable :kind :variable}
     {:name ISequential :kind :variable}
     {:name ISet :kind :variable}
     {:name ISorted :kind :variable}
     {:name IStack :kind :variable}
     {:name ISwap :kind :variable}
     {:name ITER_SYMBOL :kind :variable}
     {:name ITransientAssociative :kind :variable}
     {:name ITransientCollection :kind :variable}
     {:name ITransientMap :kind :variable}
     {:name ITransientSet :kind :variable}
     {:name ITransientVector :kind :variable}
     {:name IUUID :kind :variable}
     {:name IVector :kind :variable}
     {:name IVolatile :kind :variable}
     {:name IWatchable :kind :variable}
     {:name IWithMeta :kind :variable}
     {:name IWriter :kind :variable}
     {:name IndexedSeq :kind :variable}
     {:name IndexedSeqIterator :kind :variable}
     {:name Iterate :kind :variable}
     {:name KeySeq :kind :variable}
     {:name Keyword :kind :variable}
     {:name LazySeq :kind :variable}
     {:name List :kind :variable}
     {:name MODULE_INFOS :kind :variable}
     {:name MODULE_URIS :kind :variable}
     {:name Many :kind :variable}
     {:name MapEntry :kind :variable}
     {:name MetaFn :kind :variable}
     {:name MultiFn :kind :variable}
     {:name MultiIterator :kind :variable}
     {:name NS_CACHE :kind :variable}
     {:name Namespace :kind :variable}
     {:name NeverEquiv :kind :variable}
     {:name NodeIterator :kind :variable}
     {:name NodeSeq :kind :variable}
     {:name ObjMap :kind :variable}
     {:name PROTOCOL_SENTINEL :kind :variable}
     {:name PersistentArrayMap :kind :variable}
     {:name PersistentArrayMapIterator :kind :variable}
     {:name PersistentArrayMapSeq :kind :variable}
     {:name PersistentHashMap :kind :variable}
     {:name PersistentHashSet :kind :variable}
     {:name PersistentQueue :kind :variable}
     {:name PersistentQueueIter :kind :variable}
     {:name PersistentQueueSeq :kind :variable}
     {:name PersistentTreeMap :kind :variable}
     {:name PersistentTreeMapSeq :kind :variable}
     {:name PersistentTreeSet :kind :variable}
     {:name PersistentVector :kind :variable}
     {:name RSeq :kind :variable}
     {:name Range :kind :variable}
     {:name RangeChunk :kind :variable}
     {:name RangeIterator :kind :variable}
     {:name RangedIterator :kind :variable}
     {:name RecordIter :kind :variable}
     {:name RedNode :kind :variable}
     {:name Reduced :kind :variable}
     {:name Repeat :kind :variable}
     {:name START :kind :variable}
     {:name SeqIter :kind :variable}
     {:name Single :kind :variable}
     {:name StringBufferWriter :kind :variable}
     {:name StringIter :kind :variable}
     {:name Subvec :kind :variable}
     {:name Symbol :kind :variable}
     {:name TaggedLiteral :kind :variable}
     {:name TransformerIterator :kind :variable}
     {:name TransientArrayMap :kind :variable}
     {:name TransientHashMap :kind :variable}
     {:name TransientHashSet :kind :variable}
     {:name TransientVector :kind :variable}
     {:name UUID :kind :variable}
     {:name ValSeq :kind :variable}
     {:name Var :kind :variable}
     {:name VectorNode :kind :variable}
     {:name Volatile :kind :variable}
     {:name add-to-string-hash-cache :kind :function}
     {:name array :kind :function}
     {:name array-chunk :kind :function}
     {:name array-index-of :kind :function}
     {:name array-iter :kind :function}
     {:name array-list :kind :function}
     {:name array-seq :kind :function}
     {:name array? :kind :function}
     {:name bit-count :kind :function}
     {:name bit-shift-right-zero-fill :kind :function}
     {:name chunked-seq :kind :function}
     {:name clj->js :kind :function}
     {:name clone :kind :function}
     {:name cloneable? :kind :function}
     {:name default-dispatch-val :kind :function}
     {:name demunge :kind :function}
     {:name dispatch-fn :kind :function}
     {:name divide :kind :function}
     {:name enable-console-print! :kind :function}
     {:name equiv-map :kind :function}
     {:name es6-entries-iterator :kind :function}
     {:name es6-iterator :kind :function}
     {:name es6-iterator-seq :kind :function}
     {:name es6-set-entries-iterator :kind :function}
     {:name exists? :kind :function}
     {:name find-macros-ns :kind :function}
     {:name find-ns-obj :kind :function}
     {:name gensym_counter :kind :variable}
     {:name hash-keyword :kind :function}
     {:name hash-string :kind :function}
     {:name hash-string* :kind :function}
     {:name ifind? :kind :function}
     {:name imul :kind :function}
     {:name int-rotate-left :kind :function}
     {:name is_proto_ :kind :function}
     {:name iter :kind :function}
     {:name iterable? :kind :function}
     {:name js->clj :kind :function}
     {:name js-delete :kind :function}
     {:name js-invoke :kind :function}
     {:name js-keys :kind :function}
     {:name js-mod :kind :function}
     {:name js-obj :kind :function}
     {:name js-reserved :kind :variable}
     {:name key->js :kind :function}
     {:name key-test :kind :function}
     {:name keyword-identical? :kind :function}
     {:name m3-C1 :kind :variable}
     {:name m3-C2 :kind :variable}
     {:name m3-fmix :kind :function}
     {:name m3-hash-int :kind :function}
     {:name m3-hash-unencoded-chars :kind :function}
     {:name m3-mix-H1 :kind :function}
     {:name m3-mix-K1 :kind :function}
     {:name m3-seed :kind :variable}
     {:name missing-protocol :kind :function}
     {:name mk-bound-fn :kind :function}
     {:name native-satisfies? :kind :function}
     {:name nil-iter :kind :function}
     {:name not-native :kind :variable}
     {:name ns-interns* :kind :function}
     {:name obj-map :kind :function}
     {:name object? :kind :function}
     {:name persistent-array-map-seq :kind :function}
     {:name pr-seq-writer :kind :function}
     {:name pr-sequential-writer :kind :function}
     {:name pr-str* :kind :function}
     {:name pr-str-with-opts :kind :function}
     {:name prim-seq :kind :function}
     {:name print-map :kind :function}
     {:name print-meta? :kind :function}
     {:name print-prefix-map :kind :function}
     {:name prn-str-with-opts :kind :function}
     {:name ranged-iterator :kind :function}
     {:name reduceable? :kind :function}
     {:name regexp? :kind :function}
     {:name seq-iter :kind :function}
     {:name set-from-indexed-seq :kind :function}
     {:name set-print-err-fn! :kind :function}
     {:name set-print-fn! :kind :function}
     {:name spread :kind :function}
     {:name string-hash-cache :kind :variable}
     {:name string-hash-cache-count :kind :variable}
     {:name string-iter :kind :function}
     {:name string-print :kind :function}
     {:name symbol-identical? :kind :function}
     {:name system-time :kind :function}
     {:name this-as :kind :function}
     {:name transformer-iterator :kind :function}
     {:name truth_ :kind :function}
     {:name type->str :kind :function}
     {:name undefined? :kind :function}
     {:name uuid :kind :function}
     {:name write-all :kind :function}})

(def java-util-imports
  (->> java-util-syms
       (mapv (juxt identity #(symbol (str "java.util." (name %)))))
       (into {})))

(def common-refers->info
  {'deftest      'clojure.test
   'testing      'clojure.test
   'is           'clojure.test
   'are          'clojure.test
   'use-fixture  'clojure.test
   'run-tests    'clojure.test
   'doc          'clojure.repl
   '<!           'clojure.core.async
   '<!!          'clojure.core.async
   '>!           'clojure.core.async
   '>!!          'clojure.core.async
   'alt!         'clojure.core.async
   'alt!!        'clojure.core.async
   'alts!        'clojure.core.async
   'chan         'clojure.core.async
   'put!         'clojure.core.async
   'take!        'clojure.core.async
   'alts!!       'clojure.core.async
   'go           'clojure.core.async
   'go-loop      'clojure.core.async
   'ANY          'compojure.core
   'DELETE       'compojure.core
   'GET          'compojure.core
   'PATCH        'compojure.core
   'POST         'compojure.core
   'PUT          'compojure.core
   'context      'compojure.core
   'defroutes    'compojure.core
   'defentity    'korma.core
   'reg-event-db 're-frame.core
   'reg-sub      're-frame.core
   'reg-event-fx 're-frame.core
   'fact         'midje.sweet
   'facts        'midje.sweet})

(def common-alias->info
  {'async  'clojure.core.async
   'csv    'clojure.data.csv
   'xml    'clojure.data.xml
   'edn    'clojure.edn
   'io     'clojure.java.io
   'sh     'clojure.java.shell
   'pprint 'clojure.pprint
   'repl   'clojure.repl
   'set    'clojure.set
   'spec   'clojure.spec.alpha
   'str    'clojure.string
   'walk   'clojure.walk
   'zip    'clojure.zip})
