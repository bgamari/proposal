all : talk.pdf

figures=$(wildcard *.svg)

talk.pdf : talk.mkd $(figures:.svg=.pdf)
	pandoc -H ../header.tex --latex-engine=xelatex -F dist/build/proposal-talk/proposal-talk -t beamer -V theme=Montpellier --slide-level=2 -o $@ $<

talk.mkd : Proposal.hs
	cabal build
	dist/build/proposal-talk/proposal-talk

%.pdf : %.svg
	inkscape --export-pdf=$@ $<
