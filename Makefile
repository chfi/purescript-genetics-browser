BUILD = pulp $(FLAGS)

BUNDLE = $(BUILD) --psc-package browserify --skip-entry-point --no-check-main --standalone $(NAMESPACE) --main
NAMESPACE = GGB

OUT = ./dist/track/app.js
MODULE  = Genetics.Browser.Track.UI

$(OUT): deps
	$(BUNDLE) $(MODULE)  --to $(OUT)

build: $(OUT)

.PHONY: test
test:
	$(BUILD) test

.PHONY: clean
clean:
	rm -rf ./output \
	       ./node_modules \
         ./.psc-package \
         $(OUT)

npm   = ./node_modules
pp = ./.psc-package

$(npm):
	npm install
$(pp): $(npm)
	psc-package install

.PHONY: deps
deps: $(npm) $(pp)
