all: debug-cli

# NOTE!
#
# Older versions of the clojure launcher script may not work with this Makefile
#
# If you see errors (e.g. file not found errors) please download and install 
# the latest version of the clojure launcher script for your platform from
#
# https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools

clean:
	rm -rf cli/target \
           cli/classes \
           cli/clojure-lsp \
           cli/clojure-lsp-standalone.jar \
           lib/clojure-lsp.jar \
           clojure-lsp \
           clojure-lsp*.jar \
           docs/README.md \
           docs/CHANGELOG.md

classes:
	cd cli && clojure -T:build javac

lib-jar:
	cd lib && clojure -T:build jar
	cp -f lib/target/clojure-lsp.jar .
cli-jar: clean classes
	cd cli && clojure -X:prod-jar
	cp -f cli/clojure-lsp-standalone.jar .
cli-jar-for-native: clean classes
	cd cli && clojure -X:prod-jar-for-native
	cp -f cli/clojure-lsp-standalone.jar .

debug-cli-old: clean
	cd cli && clojure -X:javac
	cd cli && clojure -X:debug-jar
	cd cli && clojure -X:bin
	cp -f cli/clojure-lsp .

debug-cli:
	cd cli && clojure -T:build debug-cli
	cp -f cli/clojure-lsp .
prod-cli: cli-jar
	cd cli && clojure -X:bin
	cp -f cli/clojure-lsp .
native-cli: cli-jar-for-native
	cd cli && CLOJURE_LSP_JAR=clojure-lsp-standalone.jar ./graalvm/native-unix-compile.sh
	cp -f cli/clojure-lsp .

native-cli-raw-unix:
	cd cli && ./graalvm/native-unix-compile.sh
	cp -f cli/clojure-lsp .
native-cli-raw-windows:
	cd cli && ./graalvm/native-windows-compile.bat
	cp -f cli/clojure-lsp.exe .

test: classes
	cd lib && clojure -M:test
	cd cli && clojure -M:test

pod-test: classes
	cd cli && clojure -M:pod-test

integration-test:
	cd cli && bb integration-test ./clojure-lsp

lint-clean:
	cd cli && clojure -M:run clean-ns --dry --ns-exclude-regex "sample-test.*" --project-root "../"

lint-format:
	cd cli && clojure -M:run format --dry --ns-exclude-regex "sample-test.*" --project-root "../"

lint-diagnostics:
	cd cli && clojure -M:run diagnostics --ns-exclude-regex "sample-test.*" --project-root "../"

lint-fix:
	cd cli && clojure -M:run clean-ns --ns-exclude-regex "sample-test.*" --project-root "../"
	cd cli && clojure -M:run format --ns-exclude-regex "sample-test.*" --project-root "../"

release:
	./release

deploy-clojars:
	cd lib && clojure -T:build deploy-clojars

local-webpage:
	cp -rf CHANGELOG.md README.md images docs
	docker login docker.pkg.github.com
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs docker.pkg.github.com/clojure-lsp/docs-image/docs-image

.PHONY: all classes debug-cli cli-jar lib-jar cli-jar-for-native prod-cli native-cli test pod-test integration-test local-webpage clean lint-clean lint-format lint-diagnostics lint-fix release
