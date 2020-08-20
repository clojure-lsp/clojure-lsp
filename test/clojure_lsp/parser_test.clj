(ns clojure-lsp.parser-test
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.parser :as parser]
    [clojure.test :refer :all]
    [rewrite-clj.zip :as z]))

(defn syms [code]
  (->> (parser/find-usages code :clj {})
       (map :sym)
       (set)))

(defn scoped-str [code scope-bounds]
  (subs code (dec (:col scope-bounds)) (dec (:end-col scope-bounds))))

(deftest parse-destructuring-test
  (is (= '(a) (keys (parser/parse-destructuring (z/of-string "a") {} (volatile! {}) {}))))
  (is (= '(a) (keys (parser/parse-destructuring (z/of-string "[a]") {} (volatile! {}) {}))))
  (is (= '(a) (keys (parser/parse-destructuring (z/of-string "{a :a}") {} (volatile! {}) {}))))
  (is (= '(a) (keys (parser/parse-destructuring (z/of-string "{{:keys [a]} :a}") {} (volatile! {}) {}))))
  (is (= '(a b c d e) (keys (parser/parse-destructuring (z/of-string "[a {:keys [b c] :as d} e]") {} (volatile! {}) {}))))
  (is (= '(a) (keys (parser/parse-destructuring (z/of-string "[& a]") {} (volatile! {}) {}))))
  (is (= '(a c) (keys (parser/destructure-map (z/of-string "{a :b :keys [] c :d}") {} (volatile! {}) {}))))
  (is (= '(a) (keys (parser/parse-destructuring (z/of-string "{:keys [:a]}") {} (volatile! {}) {}))))
  (is (= '(a b) (keys (parser/parse-destructuring (z/of-string "{:keys [:a :b]}") {} (volatile! {}) {}))))
  (is (= '(x y) (keys (parser/parse-destructuring (z/of-string "{:keys [:a/x :b/y]}") {} (volatile! {}) {}))))
  (is (= '(x y) (keys (parser/parse-destructuring (z/of-string "{:keys [a/x b/y]}") {} (volatile! {}) {}))))
  (is (= '(x y) (keys (parser/parse-destructuring (z/of-string "{:syms [x y]}") {} (volatile! {}) {}))))
  (is (= '(x y) (keys (parser/parse-destructuring (z/of-string "{:strs [x y]}") {} (volatile! {}) {})))))

(deftest parse-bindings-test
  (let [context (volatile! {})]
    (is (= 0 (count (parser/parse-bindings (z/of-string "[]") context {} {}))))
    (is (= 1 (count (parser/parse-bindings (z/of-string "[a @db]") context {} {}))))
    (is (= 2 (count (parser/parse-bindings (z/of-string "[a [1 2 3] :let [b a]]") context {} {}))))
    (is (= 5 (count (:usages @context))))))

(deftest parse-params-test
  (let [context (volatile! {})]
    (is (= 0 (count (parser/parse-params (z/of-string "[]") context {}))))))

(deftest handle-fn-test
  (let [zloc (z/of-string "(fn [^String b])")
        context (volatile! {})]
    (parser/handle-fn (z/next zloc) zloc context {} false)
    (is (= '{"java.lang.String" #{:norename}
             "b" #{:declare :param}}
           (into {} (map (juxt (comp name :sym) :tags) (:usages @context)))))))

(deftest qualify-ident-test
  (let [context {:file-type :clj}]
    (is (= {:sym 'clojure.core/for :tags #{:norename} :str "for"} (parser/qualify-ident 'for context {} false)))
    (is (= {:sym 'java.lang.Exception :tags #{:norename} :str "Exception"} (parser/qualify-ident 'Exception context {} false)))
    (is (= {:sym 'java.lang.Exception :tags #{:norename} :str "Exception."} (parser/qualify-ident 'Exception. context {} false)))
    (is (= {:sym '.getMessage :tags #{:method :norename} :str ".getMessage"} (parser/qualify-ident '.getMessage context {} false)))
    (is (= {:sym 'java.lang.Thread/sleep :tags #{:method :norename} :str "Thread/sleep"} (parser/qualify-ident 'Thread/sleep context {} false)))
    (is (= {:sym ':foo :str ":foo"} (parser/qualify-ident :foo context {} false)))))

(deftest find-references-simple-test
  (testing "simple stuff"
    (is (= '#{clojure.core/ns clojure.core/def foo.bar/qux foo.bar// :a foo.bar}
           (syms "(ns foo.bar) (def qux qux) (def / :a)")))
    (is (= 2 (count (syms "(:id user)"))))
    (is (= 5 (count (syms "{:x id :y (:id user)}")))))

  (testing "#(dispatch-macro)"
    (let [code "#(% %1 %2 %&)"
          usages (parser/find-usages code :clj {})]
      (is (= [] (filter (fn [usage] (contains? (:tags usage) :unknown)) usages))))
    (let [code "#(+ %1) #(- %2)"
          usages (parser/find-usages code :clj {})]
      (is (= 4 (count usages)))
      (is (= [] (filter (fn [usage] (contains? (:tags usage) :unknown)) usages))))))

(deftest find-references-defn-test
  (testing "single-arity"
    (let [code "(defn a [b] b a)"
          usages (parser/find-usages code :clj {})
          bound-ref (nth usages 2 nil)
          usage-ref (nth usages 3 nil)]
      (is (= 'user/a (:sym (nth usages 1 nil))))
      (is (= #{:declare :public} (:tags (nth usages 1 nil))))
      (is (not= "user" (namespace (:sym bound-ref))))
      (is (= "b" (name (:sym bound-ref))))
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= "[b] b a)" (scoped-str code (:scope-bounds bound-ref))))
      (is (= (:sym bound-ref) (:sym usage-ref)))
      (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref)))))
  (testing "private"
    (let [code "(defn- a [b] b a) a"
          usages (parser/find-usages code :clj {})
          bound-ref (nth usages 2 nil)
          usage-ref (nth usages 3 nil)
          outside-scope-ref (nth usages 5 nil)]
      (is (= 'user/a (:sym (nth usages 1 nil))))
      (is (= #{:declare :local} (:tags (nth usages 1 nil))))
      (is (not= "user" (namespace (:sym bound-ref))))
      (is (= "b" (name (:sym bound-ref))))
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= "[b] b a)" (scoped-str code (:scope-bounds bound-ref))))
      (is (= (:sym bound-ref) (:sym usage-ref)))
      (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref)))
      (is (= "user" (namespace (:sym outside-scope-ref))))
      (is (= "a" (name (:sym outside-scope-ref))))))
  (testing "private meta"
    (let [code "(defn ^:private ^:foo a [b] b a) ^:qux a"
          usages (parser/find-usages code :clj {})
          [_ defn-ref bound-ref usage-ref _ _ outside-scope-ref] usages]
      (is (= 'user/a (:sym defn-ref)))
      (is (= #{:declare :local} (:tags defn-ref)))
      (is (not= "user" (namespace (:sym bound-ref))))
      (is (= "b" (name (:sym bound-ref))))
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= "[b] b a)" (scoped-str code (:scope-bounds bound-ref))))
      (is (= (:sym bound-ref) (:sym usage-ref)))
      (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref)))
      (is (= "user" (namespace (:sym outside-scope-ref))))
      (is (= "a" (name (:sym outside-scope-ref))))))
  (testing "private map meta"
    (let [code "(defmacro ^{:private true} ^:focus \n thing [])"
          usages (parser/find-usages code :clj {})]
      (is (= 'user/thing (:sym (nth usages 1 nil))))
      (is (= "thing" (:str (nth usages 1 nil))))
      (is (= #{:declare :local} (:tags (nth usages 1 nil))))))
  (testing "multi-arity"
    (let [code "(defn a ([b] b) ([b c] b))"
          usages (parser/find-usages code :clj {})
          def-ref (second usages)
          bound-ref (nth usages 2 nil)
          usage-ref (nth usages 3 nil)]
      (is (= 'user/a (:sym def-ref)))
      (is (not= "user" (namespace (:sym bound-ref))))
      (is (= "b" (name (:sym bound-ref))))
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= "[b] b)" (scoped-str code (:scope-bounds bound-ref))))
      (is (= (:sym bound-ref) (:sym usage-ref)))
      (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref))))))

(deftest find-references-let-test
  (testing "let"
    (let [code "(let [#_#_x 1 a 2 b 3] a)"
          usages (parser/find-usages code :clj {})
          bound-ref (second usages)
          usage-ref (nth usages 3 nil)]
      (is (not= "user" (namespace (:sym bound-ref))))
      (is (= "a" (name (:sym bound-ref))))
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= " 3] a)" (scoped-str code (:scope-bounds bound-ref))))
      (is (= (:sym bound-ref) (:sym usage-ref)))
      (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref)))))
  (testing "clojure.core/for"
    (let [code "(for [a 1 :let [b 2]] [a b])"
          usages (parser/find-usages code :clj {})
          bound-ref (second usages)
          usage-ref (nth usages 3 nil)
          b-bound (nth usages 2 nil)
          b-usage (nth usages 4 nil)]
      (is (not= "user" (namespace (:sym bound-ref))))
      (is (= "a" (name (:sym bound-ref))))
      (is (= #{:declare :param} (:tags bound-ref)))
      (is (= " [b 2]] [a b])" (scoped-str code (:scope-bounds bound-ref))))
      (is (= (:sym bound-ref) (:sym usage-ref)))
      (is (= (:scope-bounds bound-ref) (:scope-bounds usage-ref)))
      (is (= (get-in bound-ref [:scope-bounds :end-col]) (get-in b-bound [:scope-bounds :end-col])))
      (is (= (:sym b-bound) (:sym b-usage)))
      (is (= (:scope-bounds b-bound) (:scope-bounds b-usage))))
    (let [code "(for [:when true :let [a 0] b [] :let [c 0] :when true] a b c)"
          usages (parser/find-usages code :clj {})
          a-bound (nth usages 1 nil)
          a-usage (nth usages 4 nil)
          b-bound (nth usages 2 nil)
          b-usage (nth usages 5 nil)
          c-bound (nth usages 3 nil)
          c-usage (nth usages 6 nil)]
      (is (not= "user" (namespace (:sym a-bound))))
      (is (= "a" (name (:sym a-bound))))
      (is (= #{:declare :param} (:tags a-bound)))
      (is (= " :when true] a b c)" (scoped-str code (:scope-bounds c-bound))))
      (is (= (:sym a-bound) (:sym a-usage)))
      (is (= (:scope-bounds a-bound) (:scope-bounds a-usage)))
      (is (= (:sym b-bound) (:sym b-usage)))
      (is (= (:scope-bounds b-bound) (:scope-bounds b-usage)))
      (is (= (:sym c-bound) (:sym c-usage)))
      (is (= (:scope-bounds c-bound) (:scope-bounds c-usage))))))

(deftest find-references-destructuring-test
  (let [code "(let [{:keys [a] b :b} {}] a b)"
        [_ _a-kw a-param _b-kw b-param a-usage b-usage :as usages] (parser/find-usages code :clj {})]
    (is (= 7 (count usages)))
    (is (= #{:declare :param} (:tags a-param)))
    (is (= (namespace (:sym a-usage)) (namespace (:sym a-param))))
    (is (= #{:declare :param} (:tags b-param)))
    (is (= (namespace (:sym b-usage)) (namespace (:sym b-param)))))
  (let [code "(fn [y {:keys [a] b :b}] a b)"
        [_ _ _a-kw a-param _b-kw b-param a-usage b-usage :as usages] (parser/find-usages code :clj {})]
    (is (= 8 (count usages)))
    (is (= #{:declare :param} (:tags a-param)))
    (is (= (namespace (:sym a-param)) (namespace (:sym a-usage))))
    (is (= #{:declare :param} (:tags b-param)))
    (is (= (namespace (:sym b-param)) (namespace (:sym b-usage)))))
  (let [code "(fn myname [y {:keys [a] b :b}] a b)"
        [_ name-ref _ _a-kw a-param _b-kw b-param a-usage b-usage :as usages] (parser/find-usages code :clj {})]
    (is (= 9 (count usages)))
    (is (not= "user" (namespace (:sym name-ref))))
    (is (= #{:declare} (:tags name-ref)))
    (is (= #{:declare :param} (:tags a-param)))
    (is (= (namespace (:sym a-param)) (namespace (:sym a-usage))))
    (is (= #{:declare :param} (:tags b-param)))
    (is (= (namespace (:sym b-usage)) (namespace (:sym b-param))))))

(deftest find-references-ns-test
  (testing "refer all"
    (reset! db/db {:file-envs {"a.clj" [{:sym 'clojure.test :tags #{:public :ns}}
                                        {:sym 'clojure.test/deftest :tags #{:declare :public}}]}})
    (let [code "(ns foo.bar (:require [clojure.test :refer :all])) (deftest hi)"
          usages (parser/find-usages code :clj {})
          deftest-ref (nth usages 3 nil)]
      (is (= 'clojure.test/deftest (:sym deftest-ref)))))
  (testing "refers"
    (let [code "(ns foo.bar (:require [clojure.test :refer [deftest]])) (deftest hi)"
          [_ _n _r dt1 dt2 hi :as _usages] (parser/find-usages code :clj {})]
      (is (= 'clojure.test/deftest (:sym dt2)))
      (is (= #{:refer} (:tags dt1)))
      (is (not= #{:unknown} (:tags hi)))))
  (testing "import"
    (let [code "(ns foo.bar (:import java.util.jar.JarFile (java.io File))) (java.util.jar.JarFile.) (File.) (File/static 1) (JarFile.)"
          usages (drop 4 (parser/find-usages code :clj {}))
          jar-file-ref (nth usages 0 nil)
          file-ref (nth usages 1 nil)
          file-static-ref (nth usages 2 nil)
          fq-ref (nth usages 3 nil)]
      (is (= 'java.util.jar.JarFile (:sym jar-file-ref)))
      (is (= 'java.io.File (:sym file-ref)))
      (is (= 'java.io.File/static (:sym file-static-ref)))
      (is (= 'java.util.jar.JarFile (:sym fq-ref)))
      (is (= #{:norename :method} (set (mapcat :tags usages)))))))

(deftest find-references-keyword-test
  (testing "simple"
    (let [code "(ns bar (:require [qux :as q])) :foo/foo :foo ::foo ::q/foo ::x/foo"
          usages (drop 4 (parser/find-usages code :clj {}))]
      (is (= [":foo/foo" ":foo" "::foo" "::q/foo" "::x/foo"] (mapv :str usages)))
      (is (= [:foo/foo :foo :bar/foo :qux/foo] (butlast (mapv :sym usages))))
      (is (= :foo (:sym (second usages))))
      (is (nil? (seq (remove nil? (mapv :tags (butlast usages))))))
      (is (not= 'x (namespace (:sym (last usages)))))
      (is (not= 'user (namespace (:sym (last usages)))))
      (is (= #{:unknown} (:tags (last usages))))))
  (testing "in let"
    (let [code "(ns bar (:require [qux :as q]))
               (let [{:keys [:foo/a :b ::c d ::q/e ::x/f]} {}] 1)"
          [a1 a2 b1 b2 c1 c2 d1 d2 e1 e2 f1 f2 :as usages] (drop 5 (parser/find-usages code :clj {}))]
      (is (= [":foo/a" ":foo/a" ":b" ":b" "::c" "::c" "d" "d" "::q/e" "::q/e" "::x/f" "::x/f"] (mapv :str usages)))
      (is (= [:foo/a :b :bar/c :d :qux/e] (map :sym [a1 b1 c1 d1 e1])))
      (is (nil? (seq (remove nil? (mapv :tags [a1 b1 c1 d1 e1])))))
      (is (not= 'x (namespace (:sym (last usages)))))
      (is (not= 'user (namespace (:sym (last usages)))))
      (is (= #{:unknown} (:tags f1)))
      (is (= (repeat 6 #{:declare :param}) (map :tags [a2 b2 c2 d2 e2 f2])))))
  (testing "namespace ::keys"
    (let [code "(ns bar (:require [qux :as q]))
                (let [{::keys [a] :qux/keys [b] ::q/keys [c] :foo/keys [d]} {}] 1)"
          [a1 _a2 b1 _b2 c1 _c2 d1 _d2 :as usages] (drop 5 (parser/find-usages code :clj {}))]
      (is (= ["a" "a" "b" "b" "c" "c" "d" "d"] (mapv :str usages)))
      (is (= [:bar/a :qux/b :qux/c :foo/d] (map :sym [a1 b1 c1 d1]))))))

(deftest find-references-symbols-test
  (testing "simple"
    (let [code "(ns bar (:require [qux :as q])) (def foo 1) qux/foo q/foo"
          usages (parser/find-usages code :clj {})]
      (is (= ["ns" "bar" "qux" "q" "def" "foo" "qux/foo" "q/foo"] (mapv :str usages)))
      (is (= '[clojure.core/ns bar qux q clojure.core/def bar/foo qux/foo qux/foo]
             (mapv :sym usages))))))

(deftest find-loc-at-pos-test
  (is (= nil (z/sexpr (parser/loc-at-pos "  foo  " 1 1))))
  (is (= 'foo (z/sexpr (parser/loc-at-pos "  foo  " 1 3))))
  (is (= 'foo (z/sexpr (parser/loc-at-pos "  foo  " 1 5))))
  (is (= nil (z/sexpr (parser/loc-at-pos "  foo  " 1 6)))))

(deftest find-top-forms-test
  (let [code "(a) (b c d)"]
    (is (= '[(a) (b c d)]
           (->> {:row 1 :col 2 :end-row 1 :end-col (count code)}
                (parser/find-top-forms-in-range code)
                (map z/sexpr))))))

(defn gensym-counter []
  (let [cnt (atom -1)]
    (fn []
      (symbol (str "gensym" (swap! cnt inc))))))

(deftest find-references-deftype-test
  (with-redefs [gensym (gensym-counter)]
    (let [code "(deftype FooBar [fn] IBar (-x []) IQux (toString [_] (fn str)) (tooBad [a] fn) IFn)"
          usages (drop 1 (parser/find-usages code :cljs {}))]
      (is (= ['user.FooBar 'gensym0/fn
              'gensym1/IBar 'gensym2/-x 'gensym3/IQux
              'gensym4/toString 'gensym5/_ 'gensym0/fn 'cljs.core/str
              'gensym6/tooBad 'gensym7/a 'gensym0/fn
              'cljs.core/IFn]
             (mapv :sym usages))))))

(deftest find-references-cljc-test
  (let [code "(ns hi)"
        usages (parser/find-usages code :cljc {})]
    (is (= 4 (count usages)))
    (is (= 'clojure.core/ns (:sym (nth usages 0 nil))))
    (is (= 'cljs.core/ns (:sym (nth usages 2 nil))))
    (is (= "hi" (:str (nth usages 1 nil)) (:str (nth usages 3 nil)))))
  (let [code "#?(:cljs x) #?(:clj y) #?@(:clj [a b] :cljs [c d])"
        usages (parser/find-usages code :cljc {})
        clj-sexpr (z/sexpr (parser/process-reader-macro (z/of-string code) :clj))
        cljs-sexpr (z/sexpr (parser/process-reader-macro (z/of-string code) :cljs))]
    (is (= '(do y a b) clj-sexpr))
    (is (= '(do x c d) cljs-sexpr))
    (is (= 6 (count usages)))
    (is (= ["y" "a" "b"] (map :str (filter (comp #{:clj} :file-type) usages))))
    (is (= ["x" "c" "d"] (map :str (filter (comp #{:cljs} :file-type) usages))))))

(deftest if-let-test
  (let [code "(if-let [] a b)"
        usages (parser/find-usages code :clj {})]
    (is (= 3 (count usages)))))

(deftest find-references-ignored-test
  (let [code "(def x 1) (comment x (def y 2) y) #_x"
        usages (parser/find-usages code :clj {})
        def-usage (nth usages 5 nil)]
    (is (= 8 (count usages)))
    (is (= '[clojure.core/def user/x clojure.core/comment user/x clojure.core/def] (subvec (mapv :sym usages) 0 5)))
    (is (= '[user/y user/x] (subvec (mapv :sym usages) 6)))
    (is (not= 'user (namespace (:sym def-usage))))
    (is (= #{:declare} (:tags def-usage))))
  (let [code "(defn x [] #_y 1)"
        usages (parser/find-usages code :clj {})]
    (is (= 3 (count (map :sym usages)))))
  (let [code "(let [] #_y 1)"
        usages (parser/find-usages code :clj {})]
    (is (= 2 (count (map :sym usages))))))

(deftest find-references-syntax-quote
  (let [code "(defmacro x [a & body] `(def ~'a ~a ~@body))"
        usages (parser/find-usages code :clj {})]
    (is (= 7 (count usages)))
    (is (= 'clojure.core/def (:sym (nth usages 4 nil))))
    (is (= (:sym (nth usages 5 nil)) (:sym (nth usages 2 nil))))
    (is (= (:sym (nth usages 6 nil)) (:sym (nth usages 3 nil))))
    (is (= #{:scoped} (:tags (nth usages 5 nil))))
    (is (= #{:scoped} (:tags (nth usages 6 nil)))))
  (let [code "'a '(b c d) (quote e)"
        usages (parser/find-usages code :clj {})]
    (is (= 1 (count usages)))
    (is (= 'clojure.core/quote (:sym (first usages)))))
  (let [code "(def a) `(def a) (quote a) 'a '(a)"
        [d1 a1 d2 a2 q :as usages] (parser/find-usages code :clj {})]
    (is (= 5 (count usages)))
    (is (= 0 (count (filter (comp #{:unknown} :tags) usages))))
    (is (= (:sym d1) (:sym d2)))
    (is (= (:sym a1) (:sym a2)))
    (is (= 'clojure.core/quote (:sym q)))))

(deftest find-references-macro-def-test
  (testing "GET-like"
    (let [code "(def GET) (GET \"/my-route\" [a] a)"
          usages (parser/find-usages code :clj {'user/GET [:_ :function-params-and-bodies]})]
      (is (= 5 (count usages)))
      (is (= (:sym (nth usages 3 nil)) (:sym (nth usages 4 nil)))))
    (let [code "(def GET) (GET \"/my-route/:id\" {{:keys [id]} :params} id)"
          usages (parser/find-usages code :clj {'user/GET [:_ :params :bound-elements]})]
      (is (= 7 (count usages)))
      (is (= (:sym (nth usages 5 nil)) (:sym (nth usages 6 nil))))))
  (testing "for-like"
    (let [code "(def dofor) (dofor [a [1 2 3] :let [b a]] b)"
          usages (parser/find-usages code :clj {'user/dofor [:bindings :bound-elements]})]
      (is (= 7 (count usages)))
      (is (= (:sym (nth usages 3 nil)) (:sym (nth usages 5 nil))))
      (is (= (:sym (nth usages 4 nil)) (:sym (nth usages 6 nil))))))
  (testing "optional args"
    (let [optional-def {'user/mydefn [{:element :declaration
                                       :doc? true
                                       :attr-map? true}
                                      {:pred :string}
                                      {:pred :map}
                                      :function-params-and-bodies]}
          code "(def mydefn) (mydefn y \"docstring\" {:arglists [[a] [a b]]} [a] a)"
          code-noopt "(def mydefn) (mydefn y [a] a)"
          usages (drop 2 (parser/find-usages code :clj optional-def))
          usages-noopt (drop 2 (parser/find-usages code-noopt :clj optional-def))]
      (is (= 4 (count usages) (count usages-noopt)))
      (is (= (:sym (nth usages 2 nil)) (:sym (nth usages 3 nil))))
      (is (= (:sym (nth usages-noopt 2 nil)) (:sym (nth usages-noopt 3 nil))))
      (is (= "docstring" (:doc (nth usages 1 nil))))
      (is (= ["[a]" "[a b]"] (get-in (nth usages 1 nil) [:signatures :strings])))))
  (testing "deftest"
    (let [code "(ns user (:require clojure.test)) (clojure.test/deftest my-test)"
          usages (parser/find-usages code :clj {})]
      (is (= 5 (count usages)))
      (is (= '{:file-type :clj
               :sym user/my-test
               :str "my-test"
               :tags #{:declare :public :unused}}
             (dissoc (nth usages 4 nil) :col :row :end-row :end-col)))))
  (testing "as->"
    (let [code "(as-> x y (identity y))"
          usages (parser/find-usages code :clj {})]
      (is (= 5 (count usages)))
      (is (= (:sym (nth usages 2 nil)) (:sym (nth usages 4 nil))))
      (is (= {:str "y" :tags #{:declare :param}}
             (select-keys (nth usages 2 nil) [:str :tags])))))
  (testing "inside fn literal"
    (let [code "#(as-> % y (identity y))"
          usages (parser/find-usages code :clj {})]
      (is (= 5 (count usages)))
      (is (= (:sym (nth usages 2 nil)) (:sym (nth usages 4 nil))))
      (is (= {:str "y" :tags #{:declare :param}}
             (select-keys (nth usages 2 nil) [:str :tags])))))
  (testing "declare"
    (let [code "(declare a b c)"
          usages (parser/find-usages code :clj {})]
      (is (= '[clojure.core/declare user/a user/b user/c]
             (mapv :sym usages)))))
  (testing "match"
    (let [code "(ns user (:require clojure.core.match)) (clojure.core.match/match a [b] b c [c b])"
          usages (drop 5 (parser/find-usages code :clj {}))
          [s1 s2 s3 s4 s5] (map :sym usages)
          decs (map (comp boolean :declare :tags) usages)]
      (is (= [true false true false false] decs))
      (is (= s1 s2))
      (is (= s3 s4))
      (is (not= s1 s3))
      (is (not= s1 s5))))
  (testing "re-frame/reg-event-fx"
    (let [code "(defmacro reg-event-fx [& body]) (reg-event-fx :foo/name (fn [a [_ b]] b))"
          usages (parser/find-usages code :clj {'user/reg-event-fx [{:element :declaration
                                                                     :signature [:next :next :next :next]}
                                                                    :element]})]
      (is (= 10 (count usages)))
      (is (= {:tags #{:declare :public}
              :sym :foo/name
              :str ":foo/name"
              :file-type :clj
              :signatures {:sexprs ['[_ b]]
                           :strings ["[_ b]"]}}
             (dissoc (nth usages 4 nil) :col :row :end-row :end-col)))
      (is (= (:sym (nth usages 8 nil)) (:sym (nth usages 9 nil)))))))

(deftest macro-defs-subforms
  (let [code "(ns user (:require slingshot.slingshot)) (slingshot.slingshot/try+ (catch [] x x) (else foo))"
        [_ catch+ x1 x2 else :as usages] (drop 3 (parser/find-usages code :clj {}))]
    (is (= 6 (count usages)))
    (is (= 'clojure.core/catch (:sym catch+)))
    (is (= (:sym x1) (:sym x2)))
    (is (= #{:scoped} (:tags else)))))

(deftest parse-bad-reader
  (let [code "(do) ("
        usages (parser/find-usages code :clj {})]
    (is (= 0 (count usages)))))

(deftest parse-reader-tag
  (let [code "#js (inc (inc 3))"
        usages (parser/find-usages code :cljs {})]
    (is (= 2 (count usages)))))

(deftest parse-let-sexpr
  (let [code "(let [noparams] noparams)"
        usages (parser/find-usages code :clj {})]
    (is (= 3 (count usages)))
    (is (not= (:sym (last (butlast usages))) (:sym (last usages))))))

(deftest macro-def-proxy
  (let [code "(proxy [] [] (bar [a] a) (foo []) (qux [c] c))"
        usages (parser/find-usages code :clj {})
        [_ bar a-param a-use foo qux c-param c-use] usages]
    (is (= 8 (count usages)))
    (is (= #{:method :norename} (:tags bar) (:tags foo) (:tags qux)))
    (is (= (:sym a-param) (:sym a-use)))
    (is (= (:sym c-param) (:sym c-use)))
    (is (= #{:declare :param} (:tags a-param) (:tags c-param)))
    (is (= #{:scoped} (:tags a-use) (:tags c-use)))))

(deftest macro-def-reify
  (let [code "(reify String (bar [a] a) (foo []) (qux [c] c) Exception (x [f]))"
        usages (parser/find-usages code :clj {})
        [_ string-class bar a-param a-use foo qux c-param c-use ex-class _x _f-param] usages]
    (is (= 12 (count usages)))
    (is (= 'java.lang.String (:sym string-class)))
    (is (= #{:method :norename} (:tags bar) (:tags foo) (:tags qux)))
    (is (= (:sym a-param) (:sym a-use)))
    (is (= (:sym c-param) (:sym c-use)))
    (is (= #{:declare :param} (:tags a-param) (:tags c-param)))
    (is (= #{:scoped} (:tags a-use) (:tags c-use)))
    (is (= 'java.lang.Exception (:sym ex-class)))))

(deftest macro-def-defprotocol
  (testing "no docstring"
    (let [code "(defprotocol Foo (bar [a] a) (foo []) (qux [c] c))"
          usages (parser/find-usages code :clj {})
          [_ class-dec bar a-param a-use foo qux c-param c-use] usages]
      (is (= 9 (count usages)))
      (is (= 'user.Foo (:sym class-dec)))
      (is (= #{:method :declare} (:tags bar) (:tags foo) (:tags qux)))
      (is (= (:sym a-param) (:sym a-use)))
      (is (= (:sym c-param) (:sym c-use)))
      (is (= #{:declare :param} (:tags a-param) (:tags c-param)))
      (is (= #{:scoped} (:tags a-use) (:tags c-use))))
    (testing "docstring"
      (let [code "(defprotocol Foo \"I AM DOC\" (bar [a] a) (foo []) (qux [c] c))"
            usages (parser/find-usages code :clj {})
            [_ class-dec bar a-param a-use foo qux c-param c-use] usages]
        (is (= 9 (count usages)))
        (is (= 'user.Foo (:sym class-dec)))
        (is (= #{:method :declare} (:tags bar) (:tags foo) (:tags qux)))
        (is (= (:sym a-param) (:sym a-use)))
        (is (= (:sym c-param) (:sym c-use)))
        (is (= #{:declare :param} (:tags a-param) (:tags c-param)))
        (is (= #{:scoped} (:tags a-use) (:tags c-use)))))))

(deftest forward-declarations
  (let [code "(ns user (:require [clojure.spec.alpha :as s])) (s/fdef foo) (declare foo) (defn foo [])"
        usages (parser/find-usages code :clj {})
        [_ _ _ _ _ fdef-foo _ declare-foo] usages]
    (is (= #{:forward} (:tags fdef-foo)))
    (is (= #{:forward} (:tags declare-foo)))))

(deftest typed-defn
  (let [code "(ns user (:require [schema.core :as s])) (s/defn a [b c] b)"
        usages (parser/find-usages code :clj {})
        [_ u _ s _ a b c b2] usages]
    (is (= #{:declare :public} (:tags a)))
    (is (= 'user/a (:sym a)))
    (is (= nil (:doc a)))
    (is (= ["[b c]"] (get-in a [:signatures :strings])))
    (is (= (:sym b) (:sym b2)))
    (is (= [u s a b c] (filter (comp #(contains? % :declare) :tags) usages))))
  (let [code "(ns user (:require [schema.core :as s])) (s/defn a :- A \"Docs\" [b :- Long c :- [S/Str]] b)"
        usages (parser/find-usages code :clj {})
        [_ u _ s _ a _ _ b _ c _ b2] usages]
    (is (= #{:declare :public} (:tags a)))
    (is (= 'user/a (:sym a)))
    (is (= "Docs" (:doc a)))
    (is (= ["[b :- Long c :- [S/Str]]"] (get-in a [:signatures :strings])))
    (is (= (:sym b) (:sym b2)))
    (is (= [u s a b c] (filter (comp #(contains? % :declare) :tags) usages))))
  (testing "destructures param"
    (let [code "(ns user (:require [schema.core :as s])) (s/defn a :- A \"Docs\" [{b :b} :- Long c :- [S/Str]] b)"
          usages (parser/find-usages code :clj {})
          [_ u _ s _ a _ _ _ b _ c _ b2] usages]
      (is (= #{:declare :public} (:tags a)))
      (is (= 'user/a (:sym a)))
      (is (= "Docs" (:doc a)))
      (is (= ["[{b :b} :- Long c :- [S/Str]]"] (get-in a [:signatures :strings])))
      (is (= ['[{b :b} c]] (get-in a [:signatures :sexprs])))
      (is (= (:sym b) (:sym b2)))
      (is (= [u s a b c] (filter (comp #(contains? % :declare) :tags) usages)))))
  (testing "types in params are considered"
    (let [code "(ns user (:require [schema.core :as s])) (def t) (s/defn a :- t ([b :- t] b) ([c :- t d] (c d)))"
          usages (parser/find-usages code :clj {})
          [_ _u _ _s _ def-t _ _a _ t1  _b t2 _b2 _c t3 _d _c2 _d2] usages]
      (is (apply = (map :sym [def-t t1 t2 t3])))))
  (testing "handles complex return type"
    (let [code "(ns user (:require [schema.core :as s])) (s/defn a :- [A] \"Docs\" [{b :b} :- Long c :- [S/Str]] b)"
          usages (parser/find-usages code :clj {})
          [_ u _ s _ a _ _ _ b _ c _ b2] usages]
      (is (= #{:declare :public} (:tags a)))
      (is (= 'user/a (:sym a)))
      (is (= "Docs" (:doc a)))
      (is (= ["[{b :b} :- Long c :- [S/Str]]"] (get-in a [:signatures :strings])))
      (is (= (:sym b) (:sym b2)))
      (is (= [u s a b c] (filter (comp #(contains? % :declare) :tags) usages))))))

(deftest fncall-arity
  (let [code "(defn x [y] y (-> 1 x (x) (x 2)))"
        usages (parser/find-usages code :clj {})
        [_ _ _ _ _  x1 x2 x3] usages]
    (is (= 1 (:argc x1) (:argc x2)))
    (is (= 2 (:argc x3)))))

(deftest colon-symbols
  (let [code "(def foo:bar 12)"
        usages (parser/find-usages code :clj {})
        [_ foobar _] usages]
    (is (= 'user/foo:bar (:sym foobar)))))

(deftest custom-thread-macro
  (testing "cond-> should handle too few args"
    (let [code "(cond-> :always reverse)"
          [c _ _] (parser/find-usages code :clj {})]
      (is (= 2 (:argc c)))))
  (testing "my-> should tell cond-> of extra arg"
    (let [code "(defmacro my-> [& _]) (my-> 1 (cond-> :always 1))"
          [_ _ _ _ c] (parser/find-usages code :clj {'user/my-> [{:element :elements :thread-style :first}]})]
      (is (= 3 (:argc c))))))

(deftest custom-sub-forms
  (let [code "(ns f (:require [midje.sweet :as ms]))
              (ms/fact \"foo\"
                      (let [a (blah c)]
                        (bar a) => 1
                        (baz a false) =not=> nil))"
        usages (parser/find-usages code :clj {})]
    (is (= #{:scoped} (set (mapcat :tags (filter (comp #{"=>" "=not=>"} :str) usages)))))))
