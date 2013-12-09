all : proposal.pdf droplet.pdf

%.pdf : %.mkd defs.tex
	pandoc -H defs.tex --bibliography refs.bib $< -o $@

.PHONY : refs.bib
refs.bib :
	-cp ${HOME}/lori/papers/library.bib $@
	-git commit $@ -m "Update references"
