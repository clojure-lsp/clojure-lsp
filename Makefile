local-webpage :
	cp -rf README.md docs
	mkdocs build --clean

clean :
	rm -rf docs/README.md site