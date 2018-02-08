.PHONY: help default
.DEFAULT_GOAL := default
ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

default: frontend

frontend:			## Build ABS compiler (default)
	ant -buildfile $(ROOT_DIR)/build.xml dist
	@echo "Finished."
	@echo "absc command installed in frontend/bin/bash/"

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
