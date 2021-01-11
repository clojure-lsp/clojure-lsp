all:
	lein bin

test:
	lein test

local-webpage :
	cp -rf README.md images docs
	docker login docker.pkg.github.com
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs docker.pkg.github.com/clojure-lsp/docs-image/docs-image

clean :
	rm -rf target docs/README.md

.PHONY: all test local-webpage clean
