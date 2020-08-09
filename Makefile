all: dist/index.html

dist/index.html: $(shell find src -name '*.elm')
	elm make src/Main.elm --output=dist/index.html --optimize

clean:
	rm -rf dist
