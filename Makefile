all : proposal.pdf droplet.pdf

%.pdf : %.mkd defs.tex refs.bib
	pandoc -H defs.tex --bibliography refs.bib $< -o $@

.PHONY : update-refs
update-refs :
	cp ${HOME}/lori/papers/library.bib $@
	git commit $@ -m "Update references"
