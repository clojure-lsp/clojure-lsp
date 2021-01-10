local-webpage :
	cp -rf README.md docs
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs squidfunk/mkdocs-material:6.2.4

clean :
	rm -rf docs/README.md site
