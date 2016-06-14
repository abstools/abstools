# Makefile for the Ott definition of Core ABS
# Requires Ott >= 0.21.1 to be installed from http://www.cl.cam.ac.uk/~pes20/ott/
SOURCES = abs-functional-syntax.ott abs-functional-typing.ott \
	  abs-functional-evaluation.ott abs-object-syntax.ott \
	  abs-object-typing.ott abs-runtime-syntax.ott \
	  abs-runtime-typing.ott abs-runtime-evaluation.ott \
	  abs-runtime-semantics.ott
SOURCES_COQ = core-abs-simplified.ott
OTT_COQ_DIR = /usr/lib/coq/user-contrib/Ott
OTT = ott
PDFLATEX = pdflatex

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

core_abs.vo: Makefile.coq
	make -f Makefile.coq

Makefile.coq: core_abs.v
	coq_makefile -I $(OTT_COQ_DIR) core_abs.v > Makefile.coq

clean:
	rm -f core-abs.aux core-abs.log core-abs.pdf core-abs_included.aux core-abs_included.tex core_abs.v core_abs.vo core_abs.v.d core_abs.glob
