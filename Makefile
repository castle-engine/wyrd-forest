# This Makefile uses castle-engine build tool for most operations.
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

.PHONY: debug
debug:
	castle-engine compile --mode=debug

.PHONY: release
release:
	castle-engine compile --mode=release

.PHONY: clean
clean:
	castle-engine clean
	rm -Rf docs/

.PHONY: docs
docs:
	mkdir -p docs/
	pasdoc --auto-abstract code/*.pas \
	  --output docs/
