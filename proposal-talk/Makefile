all : talk.pdf

figures=$(wildcard *.svg)

dist/build/proposal-talk/proposal-talk : Filter.hs Inkscape.hs
	cabal build

talk.pdf : talk.mkd $(figures:.svg=.pdf) dist/build/proposal-talk/proposal-talk
	pandoc -H ../header.tex --latex-engine=xelatex -F dist/build/proposal-talk/proposal-talk -t beamer -V theme=Goettingen -V colortheme=default --slide-level=3 --template=template.tex -o $@ $<

%.pdf : %.svg
	inkscape --export-pdf=$@ $<
