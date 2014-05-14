PANDOC_OPTS+=--bibliography $(TOP)/refs.bib --filter=$(TOP)/filter-unicode.py
PANDOC_TEX_OPTS+=-V links-as-notes -V geometry:left=1.5in -V geometry:right=1in -V geometry:top=1in -V geometry:bottom=1in -Vdocumentclass=book -Vtoc --latex-engine=xelatex -V numbersections 
SVG_FIGURES=$(wildcard figures/*.svg)
CLEAN_FILES += $(SVG_FIGURES:.svg=.pdf)

%.pdf : %.mkd $(TOP)/header.tex $(TOP)/refs.bib $(SVG_FIGURES:.svg=.pdf)
	pandoc -H $(TOP)/header.tex $(PANDOC_OPTS) $(PANDOC_TEX_OPTS) --default-image-extension=pdf $< -o $@

%.tex : %.mkd $(TOP)/header.tex $(TOP)/refs.bib $(SVG_FIGURES:.svg=.pdf)
	pandoc -H $(TOP)/header.tex $(PANDOC_OPTS) $(PANDOC_TEX_OPTS) --default-image-extension=pdf $< -o $@

%.html : %.mkd $(TOP)/header.html $(TOP)/refs.bib $(SVG_FIGURES)
	pandoc --standalone --to=html5 --number-figures --mathjax -H $(TOP)/header.html $(PANDOC_OPTS) $(PANDOC_HTML_OPTS) --default-image-extension=svg $< -o $@

%-web : %.html $(SVG_FIGURES)
	mkdir $@ $@/figures
	cp $< $@
	cp $(SVG_FIGURES) $@/figures

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
