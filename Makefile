all: debug-bin

clean:
	rm -f clojure-lsp clojure-lsp.jar docs/README.md

debug-bin: clean
	clojure -X:debug-jar
	clojure -X:bin

prod-bin:
	clojure -X:prod-jar
	clojure -X:bin

prod-native:
	./graalvm/native-unix-compile.sh

test:
	clojure -M:test

integration-test:
	bb integration-test/run-all.clj ./clojure-lsp

local-webpage:
	cp -rf CHANGELOG.md README.md images docs
	docker login docker.pkg.github.com
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs docker.pkg.github.com/clojure-lsp/docs-image/docs-image

.PHONY: all debug-bin prod-bin prod-native test integration-test local-webpage clean
