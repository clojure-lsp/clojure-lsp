local-webpage :
	cp -rf README.md docs
	docker run --rm -v ${PWD}:/docs squidfunk/mkdocs-material:6.2.4 -- build

clean :
	rm -rf docs/README.md site
