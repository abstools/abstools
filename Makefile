.PHONY: help default frontend vagrant-vm docker clean
.DEFAULT_GOAL := default

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

default: help frontend

frontend:			## Build ABS compiler (default)
	ant -buildfile frontend/build.xml dist
	@echo "Finished."
	@echo "absc command installed in frontend/bin/bash/"

manual:				## Build the ABS manual
	mvn -B -f abs-docs clean install
	@echo "Finished."
	@echo "HTML: abs-docs/target/html/index.html"
	@echo "PDF: abs-docs/target//pdf/index.pdf"
	@echo "Epub3: abs-docs/target/epub3/index.epub"

vagrant:			## Build and start Vagrant virtual machine
	vagrant up

docker: frontend		## Build and start 'easyinterface' docker image
	$(DOCKER) build -t easyinterface .
	$(DOCKER) run -d -p 8080:80 --name easyinterface easyinterface
	@echo "Finished."
	@echo "Easyinterface running on http://localhost:8080/"

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
