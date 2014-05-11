PANDOC_OPTS+=-V links-as-notes -V geometry:left=1.5in -V geometry:right=1in -V geometry:top=1in -V geometry:bottom=1in -Vdocumentclass=book -Vtoc --latex-engine=xelatex -V numbersections
SVG_FIGURES=$(wildcard figures/*.svg)
CLEAN_FILES += $(SVG_FIGURES:.svg=.pdf)

%.pdf : %.mkd $(TOP)/defs.tex $(TOP)/refs.bib figures-pdf
	pandoc -H $(TOP)/defs.tex --bibliography $(TOP)/refs.bib $(PANDOC_OPTS) --default-image-extension=pdf $< -o $@

%.tex : %.mkd $(TOP)/defs.tex $(TOP)/refs.bib figures-pdf
	pandoc -H $(TOP)/defs.tex --bibliography $(TOP)/refs.bib $(PANDOC_OPTS) --default-image-extension=pdf $< -o $@

.PHONY : update-refs
update-refs :
	bib2bib --remove abstract --remove annote -s '$keys' ${HOME}/lori/papers/library.bib > $(TOP)/refs.bib
	git commit $(TOP)/refs.bib -m "Update references"

.PHONY : figures-pdf
figures-pdf : $(SVG_FIGURES:.svg=.pdf)

%.pdf : %.svg
	inkscape --export-pdf=$@ $<

.PHONY : clean
clean :
	rm -f $(CLEAN_FILES)
