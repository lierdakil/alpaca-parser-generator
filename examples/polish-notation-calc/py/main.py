from lexer import Lexer
from parser import Parser
from sys import argv

debug = len(argv) > 1 and argv[1] == '-d'
try:
    while True:
        line = input()
        try:
            lex = Lexer(line, debug)
            parser = Parser(lex, debug)
            expr = parser.parse()
            print(f"Result: {expr}")
        except Exception as e:
            print(f"Error while parsing: {e}")
except EOFError:
    "No-op"
