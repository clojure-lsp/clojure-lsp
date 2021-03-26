all: debug-bin

debug-bin:
	clojure -X:debug-jar
	rm -f ./clojure-lsp
	clojure -X:bin

prod-bin:
	clojure -X:prod-jar
	clojure -X:bin

test:
	clojure -M:test

integration-test:
	bb integration-test/run-all.clj ./clojure-lsp

local-webpage:
	cp -rf CHANGELOG.md README.md images docs
	docker login docker.pkg.github.com
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs docker.pkg.github.com/clojure-lsp/docs-image/docs-image

clean:
	rm -f clojure-lsp clojure-lsp.jar docs/README.md

.PHONY: all debug-bin prod-bin test integration-test local-webpage clean
