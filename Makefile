BUILD = pulp $(FLAGS)

BUNDLE = $(BUILD) browserify --skip-entry-point --no-check-main --standalone $(NAMESPACE) --main
NAMESPACE = GGB

halogen-app:	OUT = ./dist/halogen/app.js
halogen-app:	MODULE = Genetics.Browser.UI.Container

track: OUT = ./dist/track/app.js
track: MODULE  = Genetics.Browser.Track.UI

bundles = halogen-app track

$(bundles): deps
	$(BUNDLE) $(MODULE)  --to $(OUT)

build: deps
	$(BUILD) build

test: deps
	$(BUILD) test

test-browser: deps
	$(BUILD) --main Test.Main --to ./test/app.js


.PHONY: clean
clean:
	rm -rf "./output"

.PHONY: cleanbower
cleanbower:
	rm -rf "./bower_components"

.PHONY: cleannpm
cleannpm:
	rm -rf "./node_modules"

.PHONY: cleanall
cleanall:
	rm -rf ./output \
	       ./node_modules \
	       ./bower_components \

npm   = ./node_modules
bower = ./bower_components

$(npm):
	npm install
$(bower):
	bower install
deps: $(npm) $(bower)
