BUILD = spago $(FLAGS)

BUNDLE = spago bundle-module -m $(MODULE) --to temp.js

PARCEL = parcel index.html

NAMESPACE = GenomeBrowser

MODULE  = Genetics.Browser.UI

.PHONY: start
start: output
	parcel index.html

output:
	spago build

.PHONY: build
build: output
	parcel build -o index.js -d build index.js

.PHONY: test
test: deps
	$(BUILD) test


.PHONY: clean
clean:
	rm -rf ./output \
	       ./node_modules \
         ./.psc-package \
         ./dist \
         $(OUT)

npm   = ./node_modules
pp = ./.psc-package

$(npm):
	npm install
$(pp): $(npm)
	psc-package install

.PHONY: deps
deps: $(npm) $(pp)
