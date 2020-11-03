#!/usr/bin/env python3

from lexer import Lexer, TokenType
from parser import Parser
from sys import argv
from parserTypes import escape
import glob
import os

def replaceSym(s):
    return s.replace('&', '&amp;')\
        .replace('"', '&quot;')\
        .replace('<', '&lt;')\
        .replace('>', '&gt;')

debug=False
fn = argv[1]

def analyzeFile(fn):
    tokenClass = {
        'class': 'keyword',
        'primType': 'keyword',
        'propType': 'keyword',
        'methodCategory': 'keyword',
        'if': 'keyword',
        'do': 'keyword',
        'while': 'keyword',
        'let': 'keyword',
        'return': 'keyword',
        'else': 'keyword',
        'void': 'keyword',
        'var': 'keyword',
        'primVal': 'keyword',

        'identifier': 'identifier',
        'integerConstant': 'integerConstant',
        'stringConstant': 'stringConstant',

        'lb': 'symbol',
        'rb': 'symbol',
        'binOp': 'symbol',
        'equals': 'symbol',
        'minus': 'symbol',
        'not': 'symbol',
        'lp': 'symbol',
        'rp': 'symbol',
        'c': 'symbol',
        'dot': 'symbol',
        'sc': 'symbol',
        'lbr': 'symbol',
        'rbr': 'symbol',
    }


    print(f"<!-- Analyzing {fn} #-->")
    # Lexer part
    with open(fn) as f:
        lex = Lexer(f.read(), debug)
        tok, s = lex.getNextToken()
        print('<tokens>')
        while tok is not TokenType.eof:
            t = tokenClass[tok.name[4:]]
            if t == 'symbol':
                s = escape(s)
            print(f'<{t}> {s} </{t}>')
            tok, s = lex.getNextToken()
        print('</tokens>')

    # Parser part
    with open(fn) as f:
        lex = Lexer(f.read(), debug)
        parser = Parser(lex, debug)
        expr = parser.parse()
        res = "\n".join(filter(lambda x: len(x)>0, str(expr).split("\n")))
        print(res)

if os.path.isdir(fn):
    for fn in glob.glob(os.path.join(fn,'*.jack')):
        analyzeFile(fn)
else:
    analyzeFile(fn)
