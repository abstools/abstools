.PHONY: help default clean
.DEFAULT_GOAL := default

default: help all

SOURCES=$(wildcard asciidoc/*.adoc)

all: target/html/index.html	## Build documentation (default)

target/html/index.html: $(SOURCES)
	mvn install

clean:				## Remove build artifacts
	mvn clean

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
