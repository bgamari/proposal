PANDOC_OPTS=-V links-as-notes -V geometry:margin=1in --latex-engine=lualatex

all : proposal.pdf droplet.pdf

%.pdf : %.mkd defs.tex refs.bib
	pandoc -H defs.tex --bibliography refs.bib $(PANDOC_OPTS) $< -o $@

.PHONY : update-refs
update-refs :
	cp ${HOME}/lori/papers/library.bib $@
	git commit $@ -m "Update references"
