PANDOC_OPTS=-H ../header.tex --latex-engine=xelatex -t beamer -V theme=Goettingen -V colortheme=default --slide-level=3 --template=template.tex --bibliography=../refs.bib
all : talk.pdf

figures=$(wildcard *.svg)

talk.pdf : talk.mkd
	pandoc ${PANDOC_OPTS} -F ../svg-filter/dist/build/svg-filter/svg-filter -o $@ $<

notes.pdf : talk.mkd
	pandoc -t json $< -o - | ../svg-filter/dist/build/svg-filter/svg-filter notes | pandoc ${PANDOC_OPTS} -o $@ - --from json

