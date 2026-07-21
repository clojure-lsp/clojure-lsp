(ns bump-kondo
  (:require
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

(def ^:private deps-file "lib/deps.edn")
(def ^:private changelog-file "CHANGELOG.md")
(def ^:private clojars-url "https://repo.clojars.org/clj-kondo/clj-kondo")

(defn ^:private current-version [deps-content]
  (second (re-find #"clj-kondo/clj-kondo \{:mvn/version \"([^\"]+)\"" deps-content)))

(defn ^:private latest-snapshot-version
  "Returns the newest -SNAPSHOT version from clj-kondo's Clojars metadata."
  []
  (->> (slurp (str clojars-url "/maven-metadata.xml"))
       (re-seq #"<version>([^<]+)</version>")
       (map second)
       (filter #(string/ends-with? % "-SNAPSHOT"))
       sort
       last))

(defn ^:private latest-snapshot-build
  "Returns the latest timestamped build of a -SNAPSHOT version,
  e.g. 2026.05.26-20260612.132029-18 for 2026.05.26-SNAPSHOT."
  [snapshot-version]
  (let [metadata (slurp (str clojars-url "/" snapshot-version "/maven-metadata.xml"))
        [_ timestamp] (re-find #"<timestamp>([^<]+)</timestamp>" metadata)
        [_ build-number] (re-find #"<buildNumber>([^<]+)</buildNumber>" metadata)]
    (when (and timestamp build-number)
      (str (string/replace snapshot-version "-SNAPSHOT" "")
           "-" timestamp "-" build-number))))

(defn ^:private update-deps [deps-content old-version new-version]
  (string/replace deps-content
                  (str "clj-kondo/clj-kondo {:mvn/version \"" old-version "\"")
                  (str "clj-kondo/clj-kondo {:mvn/version \"" new-version "\"")))

(defn ^:private update-changelog
  "Updates the clj-kondo bump bullet in the Unreleased section, adding one
  if not present. Returns the content unchanged if there is no Unreleased section."
  [changelog-content new-version]
  (if-let [unreleased-start (string/index-of changelog-content "## Unreleased")]
    (let [new-bullet (str "- Bump clj-kondo to `" new-version "`.")
          section-end (or (string/index-of changelog-content "\n## " (inc unreleased-start))
                          (count changelog-content))
          unreleased (subs changelog-content 0 section-end)
          remaining (subs changelog-content section-end)]
      (if (re-find #"- Bump clj-kondo to `[^`]+`\." unreleased)
        (str (string/replace unreleased #"- Bump clj-kondo to `[^`]+`\." new-bullet) remaining)
        (str (string/replace-first unreleased "## Unreleased\n" (str "## Unreleased\n" new-bullet "\n")) remaining)))
    changelog-content))

(defn ^:private write-github-output! [changed? old-version new-version]
  (when-let [github-output (System/getenv "GITHUB_OUTPUT")]
    (spit github-output
          (str "changed=" changed? "\n"
               "old-version=" old-version "\n"
               "new-version=" new-version "\n")
          :append true)))

(defn bump-clj-kondo
  "Bump clj-kondo in lib/deps.edn and CHANGELOG.md to the latest snapshot
  build published on Clojars. Pass --dry-run to only check for a new version."
  [& args]
  (let [dry-run? (boolean (some #{"--dry-run"} args))
        deps-content (slurp deps-file)
        old-version (current-version deps-content)
        new-version (some-> (latest-snapshot-version) latest-snapshot-build)]
    (cond
      (not old-version)
      (do (println "Could not find current clj-kondo version in" deps-file)
          (System/exit 1))

      (not new-version)
      (do (println "Could not resolve the latest clj-kondo snapshot from Clojars.")
          (System/exit 1))

      (= old-version new-version)
      (do (println "clj-kondo already at the latest snapshot build:" old-version)
          (write-github-output! false old-version new-version))

      :else
      (do (println (str "Bumping clj-kondo " old-version " -> " new-version (when dry-run? " (dry run)")))
          (when-not dry-run?
            (spit deps-file (update-deps deps-content old-version new-version))
            (spit changelog-file (update-changelog (slurp changelog-file) new-version)))
          (write-github-output! (not dry-run?) old-version new-version)))))
