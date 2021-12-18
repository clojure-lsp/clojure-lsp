all: debug-bin

# NOTE!
#
# Older versions of the clojure launcher script may not work with this Makefile
#
# If you see errors (e.g. file not found errors) please download and install 
# the latest version of the clojure launcher script for your platform from
#
# https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools

clean:
	rm -rf classes clojure-lsp clojure-lsp.jar docs/README.md docs/CHANGELOG.md

classes:
	clojure -X:javac

debug-bin: clean classes
	clojure -X:debug-jar
	clojure -X:bin

prod-jar: clean classes
	clojure -X:prod-jar

prod-jar-for-native: clean classes
	clojure -X:prod-jar-for-native

prod-bin: clean classes prod-jar
	clojure -X:bin

prod-native:
	./graalvm/native-unix-compile.sh

test: classes
	clojure -M:test

pod-test: classes
	clojure -M:pod-test

integration-test:
	bb integration-test ./clojure-lsp

lint-clean:
	clojure -M:run clean-ns --dry --ns-exclude-regex "sample-test.*"

lint-format:
	clojure -M:run format --dry --ns-exclude-regex "sample-test.*"

lint-diagnostics:
	clojure -M:run diagnostics --ns-exclude-regex "sample-test.*"

lint-fix:
	clojure -M:run clean-ns --ns-exclude-regex "sample-test.*"
	clojure -M:run format --ns-exclude-regex "sample-test.*"

release:
	./release

local-webpage:
	cp -rf CHANGELOG.md README.md images docs
	docker login docker.pkg.github.com
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs docker.pkg.github.com/clojure-lsp/docs-image/docs-image

.PHONY: all classes debug-bin prod-jar prod-jar-for-native prod-bin prod-native test pod-test integration-test local-webpage clean release
