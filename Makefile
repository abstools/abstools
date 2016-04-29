
.PHONY: help frontend vagrant-vm docker clean
.DEFAULT_GOAL := help

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

frontend: frontend/dist/absfrontend.jar ## Build ABS compiler

manual:				## Build the ABS manual
	mvn -B -f abs-docs clean install
	@echo "Finished.  Output in abs-docs/target/classes/docs/(html,pdf,epub3/docbook)"

vagrant:			## Build and start Vagrant virtual machine
	vagrant up

docker: frontend		## Build and start 'easyinterface' docker image
	$(DOCKER) build -t easyinterface .
	$(DOCKER) run -d -p 8080:80 --name easyinterface easyinterface
	echo "Finished.  Easyinterface running on port 8080"

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

frontend/dist/absfrontend.jar:
	ant -buildfile frontend/build.xml dist
