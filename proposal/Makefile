TOP=..
CLEAN_FILES=proposal.pdf
PANDOC_TEX_OPTS=--template=template.tex -H header.tex -V include-before="\input{../signature.tex}" -F ../svg-filter/dist/build/svg-filter/svg-filter

all : proposal.pdf

include $(TOP)/rules.mk
