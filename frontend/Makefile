.PHONY: help default
.DEFAULT_GOAL := default
ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

default: frontend

frontend:			## Build ABS compiler (default)
	./gradlew clean assemble
	@echo "Finished."
	@echo "absc command installed in frontend/bin/bash/, absc.bat in frontend/bin/win"

test:				## Build ABS compiler and run all tests
	./gradlew clean build
	@echo "Finished."
	@echo "absc command installed in frontend/bin/bash/, absc.bat in frontend/bin/win"

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
