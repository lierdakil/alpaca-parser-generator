#include "parser.h"
#include <cmath>
#include <iostream>
#include <map>

int main(int argc, char *argv[]) {
  bool debug = false;
  if (argc > 1) {
    debug = (std::string(argv[1]) == "-d");
  }
  std::string line;
  while (std::getline(std::cin, line)) {
    try {
      Lexer lex(line, debug);
      Parser parser(&lex, debug);
      auto expr = parser.parse();
      std::cout << "Result: " << std::any_cast<double>(expr) << std::endl;
    } catch (std::runtime_error &e) {
      std::cerr << "Error while parsing:" << e.what() << std::endl;
    }
  }
}
