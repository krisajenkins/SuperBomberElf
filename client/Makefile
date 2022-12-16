all: dist/style.css dist/index.html

elm-files := $(shell find src -name '*.elm')
less-files := $(shell find static -name '*.less')

dist/index.html: $(elm-files)
	echo $(elm-files)
	elm-make src/App.elm --yes --warn --output $@

dist/style.css: $(less-files)
	yarn lessc $(less-files) $@
