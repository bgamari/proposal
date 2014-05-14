#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys

replacements = {
    '⁰':  '^0',
    '¹':  '^1',
    '²':  '^2',
    '³':  '^3',
    '⁴':  '^4',
    '⁵':  '^5',
    '⁶':  '^6',
    '⁷':  '^7',
    '⁸':  '^8',
    '⁹':  '^9',

    '₀':  '_0',
    '₁':  '_1',
    '₂':  '_2',
    '₃':  '_3',
    '₄':  '_4',
    '₅':  '_5',
    '₆':  '_6',
    '₇':  '_7',
    '₈':  '_8',
    '₉':  '_9',
}

for l in sys.stdin:
    for k,v in replacements.items():
        l = l.replace(k, v)
    print l,
