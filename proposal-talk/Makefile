all : talk.pdf

figures=$(wildcard *.svg)

talk.pdf : talk.mkd $(figures:.svg=.pdf)
	pandoc -H ../header.tex --latex-engine=xelatex -F ../svg-filter/dist/build/svg-filter/svg-filter -t beamer -V theme=Goettingen -V colortheme=default --slide-level=3 --template=template.tex --bibliography=../refs.bib -o $@ $<

%.pdf : %.svg
	inkscape --export-pdf=$@ $<
