.PHONY: help default clean
.DEFAULT_GOAL := default

default: help all

SOURCES=$(wildcard asciidoc/*.adoc)

all: build/asciidoc/html5/index.html ## Build documentation (default)

build/asciidoc/html5/index.html: $(SOURCES)
	../gradlew :abs-docs:assemble

clean:				## Remove build artifacts
	../gradlew :abs-docs:clean

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
