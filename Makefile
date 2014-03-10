PANDOC_OPTS=-V links-as-notes -V geometry:margin=1in -Vdocumentclass=book -Vtoc --latex-engine=lualatex
SVG_FIGURES=$(wildcard figures/*.svg)

all : proposal.pdf droplet.pdf

%.pdf : %.mkd defs.tex refs.bib figures-pdf
	pandoc -H defs.tex --bibliography refs.bib $(PANDOC_OPTS) --default-image-extension=pdf $< -o $@

.PHONY : update-refs
update-refs :
	cp ${HOME}/lori/papers/library.bib refs.bib
	git commit refs.bib -m "Update references"

.PHONY : figures-pdf
figures-pdf : $(SVG_FIGURES:.svg=.pdf)

%.pdf : %.svg
	inkscape --export-pdf=$@ $<
