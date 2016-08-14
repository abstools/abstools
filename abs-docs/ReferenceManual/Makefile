# HATS:
# Highly Adaptable and Trustworthy Software using Formal Models
# derived from
# Mobius: Mobility, Ubiquity and Security
#
# Deliverable x.x
#
# Makefile to build deliverable document

.PHONY: clean tidy help
.DEFAULT_GOAL := help

TEXSOURCES=*.tex

all: absrefmanual.pdf		## Build absrefmanual.pdf

absrefmanual.pdf: $(TEXSOURCES)
	git rev-parse --short HEAD > gitRevision.tex  ## get version no.
	pdflatex $(@:pdf=tex)
	bibtex $(@:.pdf=)
#	cd src && bibtex $(@:.pdf=)
	pdflatex $(@:pdf=tex)
	while grep "Rerun to get" $(@:pdf=log); do pdflatex $(@:pdf=tex); done

clean:				## remove build artifacts
	rm -f absrefmanual.pdf
	rm -f *.log *.aux *.bbl *.blg *.toc *.out

tidy: clean
	rm -f *.bak *~

deploy: absrefmanual.pdf
	cp absrefmanual.pdf ..

help:				## output this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

### Local Variables:
### fill-column: 70
### End:

# End of file
