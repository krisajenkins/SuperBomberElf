all: dist/index.html dist/main.css

dist/index.html: $(shell find src -type f -name '*.elm') dist
	elm make --warn --output=$@ src/App.elm

dist/%.css: static/styles/%.less
	lessc $< $@

dist:
	@mkdir $@
