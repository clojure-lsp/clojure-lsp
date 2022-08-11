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
           cli/clojure-lsp \
           cli/clojure-lsp-standalone.jar \
           lib/clojure-lsp.jar \
           clojure-lsp \
           clojure-lsp*.jar \
           docs/README.md \
           docs/CHANGELOG.md

lib-pom:
	cd lib && clojure -T:build pom
cli-pom:
	cd cli && clojure -T:build pom

lib-jar:
	cd lib && clojure -T:build jar
	mv lib/target/clojure-lsp.jar .
cli-jar:
	cd cli && clojure -T:build prod-jar
	mv cli/target/clojure-lsp-standalone.jar .
cli-jar-for-native:
	cd cli && clojure -T:build prod-jar-for-native
	mv cli/target/clojure-lsp-standalone.jar .

debug-cli:
	cd cli && clojure -T:build debug-cli
	mv cli/clojure-lsp .
prod-cli:
	cd cli && clojure -T:build prod-cli
	mv cli/clojure-lsp .
native-cli:
	cd cli && clojure -T:build native-cli
	mv cli/clojure-lsp .

test:
	cd lib && clojure -M:test
	cd cli && clojure -M:test

pod-test:
	cd cli && clojure -M:pod-test

integration-test:
	cd cli && bb integration-test ../clojure-lsp

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
deploy-clojars-standalone:
	cd cli && clojure -T:build deploy-clojars

local-webpage:
	cp -rf CHANGELOG.md README.md images docs
	docker login docker.pkg.github.com
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs docker.pkg.github.com/clojure-lsp/docs-image/docs-image

.PHONY: all debug-cli cli-jar lib-jar cli-jar-for-native prod-cli native-cli test pod-test integration-test local-webpage clean lint-clean lint-format lint-diagnostics lint-fix release
