# Makefile for the Ott definition of Core ABS
# Requires Ott >= 0.21.1 to be installed from http://www.cl.cam.ac.uk/~pes20/ott/
SOURCES = abs-functional-syntax.ott abs-functional-typing.ott \
	  abs-functional-evaluation.ott abs-object-syntax.ott \
	  abs-object-typing.ott abs-runtime-syntax.ott \
	  abs-runtime-typing.ott abs-runtime-evaluation.ott \
	  abs-runtime-semantics.ott
SOURCES_COQ = core-abs-simplified.ott
OTT = ott
PDFLATEX = pdflatex

.PHONY: default all pdf coq help clean

.DEFAULT_GOAL := default

default: help pdf

all: pdf coq			## Create both pdf and coq

pdf: core-abs.pdf core-abs-coq.pdf ## Type-set ABS semantic rules (default)

coq: core_abs.v		## Generate coq proof

core-abs.pdf: core-abs.tex core-abs_included.tex
	$(PDFLATEX) core-abs.tex

core-abs_included.tex: $(SOURCES)
	$(OTT) -tex_wrap false \
	       -tex_name_prefix coreabs \
	       -merge true \
	       -o core-abs_included.tex \
	       $(SOURCES)

core-abs-coq.tex: $(SOURCES_COQ)
	$(OTT) -o core-abs-coq.tex \
	       $(SOURCES_COQ)

core-abs-coq.pdf: core-abs-coq.tex
	$(PDFLATEX) core-abs-coq.tex

core_abs.v: $(SOURCES_COQ)
	$(OTT) -o core_abs.v \
	       -coq_expand_list_types false \
	       $(SOURCES_COQ)

clean:				## Remove intermediate and output files
	rm -f *.aux *.log core-abs.pdf core-abs-coq.pdf core-abs_included.tex core-abs-coq.tex core_abs.v core_abs.vo core_abs.v.d core_abs.glob Makefile.coq Makefile.coq.conf

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
