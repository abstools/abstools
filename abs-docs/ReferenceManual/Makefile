# HATS: 
# Highly Adaptable and Trustworthy Software using Formal Models
# derived from
# Mobius: Mobility, Ubiquity and Security
#
# Deliverable x.x
#
# Makefile to build deliverable document

.PHONY: clean tidy

TEXSOURCES=*.tex

absrefmanual.pdf: $(TEXSOURCES)
	git rev-parse --short HEAD > gitRevision.tex  ## get version no.
	pdflatex $(@:pdf=tex)
	bibtex $(@:.pdf=)
#	cd src && bibtex $(@:.pdf=)
	pdflatex $(@:pdf=tex)
	while grep "Rerun to get" $(@:pdf=log); do pdflatex $(@:pdf=tex); done

clean:
	rm -f absrefmanual.pdf
	rm -f *.log *.aux *.bbl *.blg *.toc *.out 

tidy: clean
	rm -f *.bak *~

deploy: absrefmanual.pdf 
	cp absrefmanual.pdf ..

### Local Variables: 
### fill-column: 70
### End: 

# End of file
