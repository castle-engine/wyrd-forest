# This Makefile uses castle-engine build tool for most operations.
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

MODE=debug

.PHONY: standalone
standalone:
	castle-engine compile --mode=$(MODE)

.PHONY: clean
clean:
	castle-engine clean
	rm -Rf docs/

.PHONY: docs
docs:
	mkdir -p docs/
	pasdoc --auto-abstract code/*.pas \
	  --output docs/
