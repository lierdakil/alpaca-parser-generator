from lexer import lex
from parser import Parser
from sys import argv

debug = len(argv) > 1 and argv[1] == '-d'
try:
    while True:
        line = input()
        try:
            parser = Parser(debug)
            toks = lex(line, debug)
            expr = parser.parse(toks)
            print(f"Result: {expr}")
        except Exception as e:
            print(f"Error while parsing: {e}")
            raise e
except EOFError:
    "No-op"
