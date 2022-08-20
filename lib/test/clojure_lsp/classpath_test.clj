(ns clojure-lsp.classpath-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [clojure-lsp.classpath :as classpath]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.test-helper :as h]
   [clojure.test :refer [deftest is testing]]))

(deftest default-project-specs-test
  (with-redefs [classpath/locate-executable identity]

    (testing "empty source-aliases"
      (is (h/assert-contains-submaps
            [{:project-path "project.clj"
              :classpath-cmd ["lein" "classpath"]}
             {:project-path "deps.edn"
              :classpath-cmd ["clojure" "-Spath"]}
             {:project-path "build.boot"
              :classpath-cmd ["boot" "show" "--fake-classpath"]}
             {:project-path "shadow-cljs.edn"
              :classpath-cmd ["npx" "shadow-cljs" "classpath"]}
             {:project-path "bb.edn"
              :classpath-cmd ["bb" "print-deps" "--format" "classpath"]}]
            (classpath/default-project-specs #{}))))
    (testing "single source-alias"
      (is (h/assert-contains-submaps
            [{:project-path "project.clj"
              :classpath-cmd ["lein" "with-profile" "+something" "classpath"]}
             {:project-path "deps.edn"
              :classpath-cmd ["clojure" "-A:something" "-Spath"]}
             {:project-path "build.boot"
              :classpath-cmd ["boot" "show" "--fake-classpath"]}
             {:project-path "shadow-cljs.edn"
              :classpath-cmd ["npx" "shadow-cljs" "classpath"]}
             {:project-path "bb.edn"
              :classpath-cmd ["bb" "print-deps" "--format" "classpath"]}]
            (classpath/default-project-specs #{:something}))))
    (testing "multiple source-aliases"
      (is (h/assert-contains-submaps
            [{:project-path "project.clj"
              :classpath-cmd ["lein" "with-profile" "+otherthing,+something" "classpath"]}
             {:project-path "deps.edn"
              :classpath-cmd ["clojure" "-A:otherthing:something" "-Spath"]}
             {:project-path "build.boot"
              :classpath-cmd ["boot" "show" "--fake-classpath"]}
             {:project-path "shadow-cljs.edn"
              :classpath-cmd ["npx" "shadow-cljs" "classpath"]}
             {:project-path "bb.edn"
              :classpath-cmd ["bb" "print-deps" "--format" "classpath"]}]
            (classpath/default-project-specs #{:something :otherthing}))))))

(defn make-components
  "Create a PROJECT-FILENAME file at DIR and return a clojure-lsp
components reference to it."
  [dir project-filename]
  (let [project (.toString (fs/path dir project-filename))
        db {:project-root-uri (-> dir .toUri .toString)
            :settings {:project-specs
                       (classpath/default-project-specs #{})}}]
    (spit project "")
    {:db* (atom db)}))

(defn locate-executable-mock
  "Return a fn that mocks `clojure-lsp.classpath/locate-executable` by
  returning EXECUTABLE paths as found in the static RESPONSES map."
  [responses]
  (fn [executable]
    (some (fn [[exec response]]
            (when (= executable exec)
              response))
          responses)))

(defn shell-mock
  "Return a fn that mocks `clojure-lsp.classpath/shell` by returning a
  succesfull exit code with a corresponding value from RESPONSES when
  the CMD-AND-ARGS match a RESPONSES key or returns a failed exit code
  if not."
  [responses]
  (fn [& cmd-and-args]
    (if-let [out (some (fn [[response-key response]]
                         (when (= response-key cmd-and-args)
                           response))
                       responses)]
      {:exit 0 :out out}

      {:exit 1})))

(deftest classpath

  (testing "babashka"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [classpath/locate-executable (locate-executable-mock {"bb" "pathto/bb.xyz"})

                    classpath/shell
                    (shell-mock {["pathto/bb.xyz" "print-deps" "--format" "classpath" :dir (.toString temp-dir)]
                                 (str/join fs/path-separator ["a" "b"])})]

        (let [components (make-components temp-dir "bb.edn")]
          (is (= #{"a" "b"} (classpath/scan-classpath! components)))))))

  (testing "boot"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [classpath/locate-executable (locate-executable-mock {"boot" "pathto/boot.xyz"})

                    classpath/shell
                    (shell-mock {["pathto/boot.xyz" "show" "--fake-classpath" :dir (.toString temp-dir)]
                                 (str/join fs/path-separator ["a" "b"])})]
        (let [components (make-components temp-dir "build.boot")]
          (is (= #{"a" "b"} (classpath/scan-classpath! components)))))))

  (testing "clojure"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [shared/windows-os? false

                    classpath/locate-executable (locate-executable-mock {"clojure" "pathto/clojure.xyz"})

                    classpath/shell
                    (shell-mock {["pathto/clojure.xyz" "-Spath" :dir (.toString temp-dir)]
                                 (str/join fs/path-separator ["a" "c"])})]

        (let [components (make-components temp-dir "deps.edn")]
          (is (= #{"a" "c"} (classpath/scan-classpath! components)))))))

  ;; exhaustive testing of powershell invocation on windows

  (testing "clojure win [powershell clojure]"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [shared/windows-os? true

                    classpath/locate-executable (locate-executable-mock {"powershell" "pathto/powershell.exe"})

                    classpath/shell
                    (shell-mock {(#'classpath/psh-cmd "pathto/powershell.exe" "Get-Command" "clojure")
                                 "clojure command exists"

                                 (#'classpath/psh-cmd "pathto/powershell.exe" "clojure" "-Spath" :dir (.toString temp-dir))
                                 (str/join fs/path-separator ["a" "c"])})]

        (let [components (make-components temp-dir "deps.edn")]
          (is (= #{"a" "c"} (classpath/scan-classpath! components)))))))

  (testing "clojure win [pwsh clojure]"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [shared/windows-os? true

                    classpath/locate-executable (locate-executable-mock {"pwsh" "pathto/pwsh.exe"})

                    classpath/shell
                    (shell-mock {(#'classpath/psh-cmd "pathto/pwsh.exe" "Get-Command" "clojure")
                                 "clojure command exists"

                                 (#'classpath/psh-cmd "pathto/pwsh.exe" "clojure" "-Spath" :dir (.toString temp-dir))
                                 (str/join fs/path-separator ["a" "c"])})]

        (let [components (make-components temp-dir "deps.edn")]
          (is (= #{"a" "c"} (classpath/scan-classpath! components)))))))

  (testing "clojure win [powershell clojure] & [pwsh clojure]"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [shared/windows-os? true

                    classpath/locate-executable (locate-executable-mock {"powershell" "pathto/powershell.exe"
                                                                         "pwsh" "pathto/pwsh.exe"})

                    classpath/shell
                    (shell-mock {(#'classpath/psh-cmd "pathto/powershell.exe" "Get-Command" "clojure")
                                 "clojure command exists"
                                 (#'classpath/psh-cmd "pathto/pwsh.exe" "Get-Command" "clojure")
                                 "clojure command exists"

                                 (#'classpath/psh-cmd "pathto/powershell.exe" "clojure" "-Spath" :dir (.toString temp-dir))
                                 (str/join fs/path-separator ["a" "c"])})]

        (let [components (make-components temp-dir "deps.edn")]
          (is (= #{"a" "c"} (classpath/scan-classpath! components)))))))

  (testing "clojure win [powershell clojure] & [pwsh]"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [shared/windows-os? true

                    classpath/locate-executable (locate-executable-mock {"powershell" "pathto/powershell.exe"
                                                                         "pwsh" "pathto/pwsh.exe"})

                    classpath/shell
                    (shell-mock {(#'classpath/psh-cmd "pathto/powershell.exe" "Get-Command" "clojure")
                                 "clojure command exists"

                                 (#'classpath/psh-cmd "pathto/powershell.exe" "clojure" "-Spath" :dir (.toString temp-dir))
                                 (str/join fs/path-separator ["a" "c"])})]

        (let [components (make-components temp-dir "deps.edn")]
          (is (= #{"a" "c"} (classpath/scan-classpath! components)))))))

  (testing "clojure win [powershell] & [pwsh clojure]"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [shared/windows-os? true

                    classpath/locate-executable (locate-executable-mock {"powershell" "pathto/powershell.exe"
                                                                         "pwsh" "pathto/pwsh.exe"})

                    classpath/shell
                    (shell-mock {(#'classpath/psh-cmd "pathto/pwsh.exe" "Get-Command" "clojure")
                                 "clojure command exists"

                                 (#'classpath/psh-cmd "pathto/pwsh.exe" "clojure" "-Spath" :dir (.toString temp-dir))
                                 (str/join fs/path-separator ["a" "c"])})]

        (let [components (make-components temp-dir "deps.edn")]
          (is (= #{"a" "c"} (classpath/scan-classpath! components)))))))


  ;; test with multiple project files present
  (testing "clojure & bb"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [shared/windows-os? false

                    classpath/locate-executable (locate-executable-mock {"bb" "pathto/bb.xyz"
                                                                         "clojure" "pathto/clojure.xyz"})

                    classpath/shell
                    (shell-mock {["pathto/bb.xyz" "print-deps" "--format" "classpath" :dir (.toString temp-dir)]
                                 (str/join fs/path-separator ["a" "b"])

                                 ["pathto/clojure.xyz" "-Spath" :dir (.toString temp-dir)]
                                 (str/join fs/path-separator ["a" "c"])})]

        (let [components (make-components temp-dir "deps.edn")
              bb (.toString (fs/path temp-dir "bb.edn"))]
          (spit bb "")
          (is (= #{"a" "b" "c"} (classpath/scan-classpath! components)))))))

  (testing "lein"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [shared/windows-os? false
                    classpath/locate-executable (locate-executable-mock {"lein" "pathto/lein.xyz"})
                    classpath/shell (shell-mock {["pathto/lein.xyz" "classpath" :dir (.toString temp-dir)]
                                                 (str/join fs/path-separator ["a" "b"])})]
        (let [components (make-components temp-dir "project.clj")]
          (is (= #{"a" "b"} (classpath/scan-classpath! components)))))))

  ;; lein can be either a standalone script or invoked via powershell.
  (testing "lein win"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [shared/windows-os? true
                    classpath/locate-executable (locate-executable-mock {"lein" "pathto/lein.xyz"})
                    classpath/shell (shell-mock {["pathto/lein.xyz" "classpath" :dir (.toString temp-dir)]
                                                 (str/join fs/path-separator ["a" "b"])})]
        (let [components (make-components temp-dir "project.clj")]
          (is (= #{"a" "b"} (classpath/scan-classpath! components)))))))

  ;; just run a sample of powershell invocation tests, exhaustive testing is done via clojure
  (testing "lein win [pwsh clojure]"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [shared/windows-os? true

                    classpath/locate-executable (locate-executable-mock {"pwsh" "pathto/pwsh.exe"})

                    classpath/shell
                    (shell-mock {(#'classpath/psh-cmd "pathto/pwsh.exe" "Get-Command" "lein")
                                 "lein command exists"

                                 (#'classpath/psh-cmd "pathto/pwsh.exe" "lein" "classpath" :dir (.toString temp-dir))
                                 (str/join fs/path-separator ["a" "c"])})]

        (let [components (make-components temp-dir "project.clj")]
          (is (= #{"a" "c"} (classpath/scan-classpath! components)))))))

  (testing "shadow-cljs"
    (fs/with-temp-dir
      [temp-dir {}]
      (with-redefs [classpath/locate-executable (locate-executable-mock {"npx" "pathto/npx.xyz"})

                    classpath/shell
                    (shell-mock {["pathto/npx.xyz" "shadow-cljs" "classpath" :dir (.toString temp-dir)]
                                 (str/join fs/path-separator ["a" "b"])})]

        (let [components (make-components temp-dir "shadow-cljs.edn")]
          (is (= #{"a" "b"} (classpath/scan-classpath! components))))))))
