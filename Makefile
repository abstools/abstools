.PHONY: help default frontend docker clean server
.DEFAULT_GOAL := default

# Check that given variables are set and all have non-empty values, die with
# an error otherwise.  See
# https://stackoverflow.com/questions/10858261/abort-makefile-if-variable-not-set
#
# Params:
#   1. Variable name(s) to test.
#   2. (optional) Error message to print.
check_defined = \
    $(strip $(foreach 1,$1, \
        $(call __check_defined,$1,$(strip $(value 2)))))
__check_defined = \
    $(if $(value $1),, \
        $(error Undefined $1$(if $2, ($2))$(if $(value @), \
                required by target `$@`)))

# https://stackoverflow.com/questions/18136918/how-to-get-current-relative-directory-of-your-makefile#18137056
ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))


ifneq (,$(findstring /cygdrive/,$(PATH)))
    UNAME := Cygwin
else
ifneq (,$(findstring WINDOWS,$(PATH)))
    UNAME := Windows
else
    UNAME := $(shell uname -s)
endif
endif

ifeq (,$(DOCKER))
	ifeq ($(UNAME), Linux)
		DOCKER := "sudo docker"
	else
		DOCKER := docker
	endif
endif

default: help frontend manual

frontend:			## Build ABS compiler (default)
	$(MAKE) -C $(ROOT_DIR)/frontend

manual:				## Build the ABS manual
	./gradlew asciidoc
	@echo "Finished."
	@echo "HTML: abs-docs/build/asciidoc/html5/index.html"
	@echo "PDF: abs-docs/build/asciidoc/pdf/index.pdf"

docker:				## Build docker images for collaboratory and absc
	$(DOCKER) pull erlang:26-alpine
	$(DOCKER) pull php:7.4-apache-buster
	$(DOCKER) build -t abslang/collaboratory -f docker/collaboratory.Dockerfile $(ROOT_DIR)
	$(DOCKER) build -t abslang/absc -f docker/absc.Dockerfile $(ROOT_DIR)
	@echo "Finished."

run-collaboratory:		## Run the collaboratory on port 8080
	$(DOCKER) run -d -p 8080:80 --name collaboratory abslang/collaboratory
	@echo "Collaboratory running on http://localhost:8080/"

server:				## Deploy development environment on Debian-based server
	@:$(call check_defined, SERVER, server name or address as accepted by ssh)
	ssh $(SERVER) sudo apt-get -y update
	ssh $(SERVER) sudo apt-get -y install make openjdk-21-jdk openjdk-21-jre erlang maude emacs git
	ssh $(SERVER) git clone https://github.com/abstools/abstools
	ssh $(SERVER) make -f "~/abstools/Makefile" frontend
	ssh $(SERVER) 'echo "PATH=\$$HOME/abstools/frontend/bin/bash:\$$PATH" >> ~/.bashrc'

clean:				## Remove all build artifacts
	./gradlew clean

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
