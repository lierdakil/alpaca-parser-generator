#include "lexer.h"
#include "parser.h"
#include <any>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <regex>
#include <string>

#define debug false

std::string tokenClass(TokenType t) {
  switch (t) {
  case TokenType::eof:
    return "";
  case TokenType::Tok_binOp:
    return "symbol";
  case TokenType::Tok_c:
    return "symbol";
  case TokenType::Tok_class:
    return "keyword";
  case TokenType::Tok_do:
    return "keyword";
  case TokenType::Tok_dot:
    return "symbol";
  case TokenType::Tok_else:
    return "keyword";
  case TokenType::Tok_equals:
    return "symbol";
  case TokenType::Tok_identifier:
    return "identifier";
  case TokenType::Tok_if:
    return "keyword";
  case TokenType::Tok_integerConstant:
    return "integerConstant";
  case TokenType::Tok_lb:
    return "symbol";
  case TokenType::Tok_lbr:
    return "symbol";
  case TokenType::Tok_let:
    return "keyword";
  case TokenType::Tok_lp:
    return "symbol";
  case TokenType::Tok_methodCategory:
    return "keyword";
  case TokenType::Tok_minus:
    return "symbol";
  case TokenType::Tok_not:
    return "symbol";
  case TokenType::Tok_primType:
    return "keyword";
  case TokenType::Tok_primVal:
    return "keyword";
  case TokenType::Tok_propType:
    return "keyword";
  case TokenType::Tok_rb:
    return "symbol";
  case TokenType::Tok_rbr:
    return "symbol";
  case TokenType::Tok_return:
    return "keyword";
  case TokenType::Tok_rp:
    return "symbol";
  case TokenType::Tok_sc:
    return "symbol";
  case TokenType::Tok_stringConstant:
    return "stringConstant";
  case TokenType::Tok_var:
    return "keyword";
  case TokenType::Tok_void:
    return "keyword";
  case TokenType::Tok_while:
    return "keyword";
  }
}

void analyzeFile(std::string fn) {
  std::cout << "<!-- Analyzing " << fn << " #-->\n";

  // Lexer part
  std::ifstream t(fn);
  std::string content((std::istreambuf_iterator<char>(t)),
                      std::istreambuf_iterator<char>());
  auto lex = new Lexer(content, debug);
  auto [tok, s] = lex->getNextToken();
  std::cout << "<tokens>\n";
  while (tok != TokenType::eof) {
    auto t = tokenClass(tok);
    auto str = std::any_cast<std::string>(s);
    if (t == "symbol")
      str = escape(str);
    else if (t == "stringConstant") {
      auto a = str.begin();
      a++;
      auto b = str.end();
      b--;
      str = std::string(a, b);
    }
    std::cout << "<" << t << ">" << str << "</" << t << ">\n";
    auto x = lex->getNextToken();
    tok = x.first;
    s = x.second;
  }
  std::cout << "</tokens>\n";

  // Parser part
  auto lex1 = new Lexer(content, debug);
  auto parser = new Parser(lex1, debug);
  std::cout << std::any_cast<std::string>(parser->parse());
}

int main(int argc, char *argv[]) {
  if (argc <= 1) {
    std::cerr << "argument required\n";
    return 1;
  }
  auto fn = argv[1];
  const std::string ext = ".jack";
  if (std::filesystem::is_directory(fn)) {
    for (auto &i : std::filesystem::directory_iterator(fn)) {
      if (std::equal(ext.rbegin(), ext.rend(), i.path().string().rbegin())) {
        analyzeFile(i.path());
      }
    }
  } else {
    analyzeFile(fn);
  }

  return 0;
}
