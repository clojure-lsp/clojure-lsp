all: debug-bin

debug-bin:
	clj -X:debug-jar
	clj -X:bin

prod-bin:
	clj -X:prod-jar
	clj -X:bin

test:
	clj -M:test

local-webpage:
	cp -rf CHANGELOG.md README.md images docs
	docker login docker.pkg.github.com
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs docker.pkg.github.com/clojure-lsp/docs-image/docs-image

clean:
	rm -f clojure-lsp clojure-lsp.jar docs/README.md

.PHONY: all test local-webpage clean
