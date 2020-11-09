#include "format.h"
#include <regex>
#include <sstream>

str escape(strr s) {
  auto ts = std::regex_replace(s, std::regex("&"), "&amp;");
  ts = std::regex_replace(ts, std::regex("\""), "&quot;");
  ts = std::regex_replace(ts, std::regex("<"), "&lt;");
  ts = std::regex_replace(ts, std::regex(">"), "&gt;");
  return ts;
}

void join(ss &t, lst l) {
  for (auto &p : l)
    t << p;
}

template <typename It> void joinDot(ss &t, It begin, It end) {
  if (begin == end)
    return;
  t << *begin;
  ++begin;
  for (; begin != end; ++begin) {
    t << "<symbol> . </symbol>\n";
    t << *begin;
  }
}

template <typename It> void joinComma(ss &t, It begin, It end) {
  if (begin == end)
    return;
  t << *begin;
  ++begin;
  for (; begin != end; ++begin) {
    t << "<symbol> , </symbol>\n";
    t << *begin;
  }
}

lst prepend(lst l, strr s) {
  l.push_front(s);
  return l;
}

str ArrayExpr(strr name, strr idx) {
  ss t;
  t << name;
  t << "<symbol> [ </symbol>\n";
  t << "<expression>\n";
  t << idx;
  t << "</expression>\n";
  t << "<symbol> ] </symbol>\n";
  return t.str();
}

str Class(strr name, lpairr def) {
  ss t;
  t << "<class>\n";
  t << "<keyword> class </keyword>\n";
  t << name;
  t << "<symbol> { </symbol>\n";
  join(t, def.first);
  join(t, def.second);
  t << "<symbol> } </symbol>\n";
  t << "</class>\n";
  return t.str();
}

str BinOp(strr op, strr a, strr b) {
  ss t;
  t << a;
  t << "<symbol> " << escape(op) << "</symbol>\n";
  t << b;
  return t.str();
}
str Call(lstr method, lstr arguments) {
  ss t;
  joinDot(t, method.cbegin(), method.cend());
  t << "<symbol> ( </symbol>\n";
  t << "<expressionList>\n";
  if (arguments.size() > 0) {
    auto a1 = arguments.cbegin();
    t << "<expression>\n" << *a1 << "</expression>\n";
    ++a1;
    for (auto end = arguments.cend(); a1 != end; ++a1) {
      t << "<symbol> , </symbol>\n";
      t << "<expression>\n" << *a1 << "</expression>\n";
    }
  }
  t << "</expressionList>\n";
  t << "<symbol> ) </symbol>\n";
  return t.str();
}
str IntConst(strr value) {
  ss t;
  t << "<integerConstant>" << value << "</integerConstant>\n";
  return t.str();
}
str Term(strr what) {
  ss t;
  t << "<term>\n";
  t << what;
  t << "</term>\n";
  return t.str();
}
str Parens(strr expr) {
  ss t;
  t << "<term>\n";
  t << "<symbol> ( </symbol>\n";
  t << "<expression>\n";
  t << expr;
  t << "</expression>\n";
  t << "<symbol> ) </symbol>\n";
  t << "</term>\n";
  return t.str();
}
str Neg(strr expr) {
  ss t;
  t << "<term>\n";
  t << "<symbol> - </symbol>\n";
  t << expr;
  t << "</term>\n";
  return t.str();
}
str Not(strr expr) {
  ss t;
  t << "<term>\n";
  t << "<symbol> ~ </symbol>\n";
  t << expr;
  t << "</term>\n";
  return t.str();
}
str PrimVal(strr val) {
  ss t;
  t << "<keyword>" << val << "</keyword>\n";
  return t.str();
}
str StrConst(strr s) {
  ss t;
  auto a = s.begin();
  a++;
  auto b = s.end();
  b--;
  auto val = std::string(a, b);
  t << "<stringConstant>" << val << "</stringConstant>\n";
  return t.str();
}
str Identifier(strr name) {
  ss t;
  t << "<identifier>" << name << "</identifier>\n";
  return t.str();
}
str Body(lstr local, lstr statements) {
  ss t;
  join(t, local);
  t << "<statements>\n";
  join(t, statements);
  t << "</statements>\n";
  return t.str();
}
str DoStmt(strr expr) {
  ss t;
  t << "<doStatement>\n";
  t << "<keyword> do </keyword>\n";
  t << expr;
  t << "<symbol> ; </symbol>\n";
  t << "</doStatement>\n";
  return t.str();
}
str IfStmt(strr cond, lstr iftru, lstr iffls) {
  ss t;
  t << "<ifStatement>\n";
  t << "<keyword> if </keyword>\n";
  t << "<symbol> ( </symbol>\n";
  t << "<expression>\n";
  t << cond;
  t << "</expression>\n";
  t << "<symbol> ) </symbol>\n";
  t << "<symbol> { </symbol>\n";
  t << "<statements>\n";
  join(t, iftru);
  t << "</statements>\n";
  t << "<symbol> } </symbol>\n";
  if (iffls.size() > 0) {
    t << "<keyword> else </keyword>\n";
    t << "<symbol> { </symbol>\n";
    t << "<statements>\n";
    join(t, iffls);
    t << "</statements>\n";
    t << "<symbol> } </symbol>\n";
  }
  t << "</ifStatement>\n";
  return t.str();
}
str LetStmt(strr var, strr expr) {
  ss t;
  t << "<letStatement>\n";
  t << "<keyword> let </keyword>\n";
  t << var;
  t << "<symbol> = </symbol>\n";
  t << "<expression>\n";
  t << expr;
  t << "</expression>\n";
  t << "<symbol> ; </symbol>\n";
  t << "</letStatement>\n";
  return t.str();
}
str ReturnStmt(strr expr) {
  ss t;
  t << "<returnStatement>\n";
  t << "<keyword> return </keyword>\n";
  if (expr.length() > 0) {
    t << "<expression> " << expr << " </expression>\n";
  }
  t << "<symbol> ; </symbol>\n";
  t << "</returnStatement>\n";
  return t.str();
}
str WhileStmt(strr cond, lstr body) {
  ss t;
  t << "<whileStatement>\n";
  t << "<keyword> while </keyword>\n";
  t << "<symbol> ( </symbol>\n";
  t << "<expression>\n";
  t << cond;
  t << "</expression>\n";
  t << "<symbol> ) </symbol>\n";
  t << "<symbol> { </symbol>\n";
  t << "<statements>\n";
  join(t, body);
  t << "</statements>\n";
  t << "<symbol> } </symbol>\n";
  t << "</whileStatement>\n";
  return t.str();
}
str MethodDef(strr cat, strr type, strr name, lstr params, strr body) {
  ss t;
  t << "<subroutineDec>\n";
  t << "<keyword> " << cat << " </keyword>\n";
  t << type;
  t << name;
  t << "<symbol> ( </symbol>\n";
  t << "<parameterList>\n";
  joinComma(t, params.cbegin(), params.cend());
  t << "</parameterList>\n";
  t << "<symbol> ) </symbol>\n";
  t << "<subroutineBody>\n";
  t << "<symbol> { </symbol>\n";
  t << body;
  t << "<symbol> } </symbol>\n";
  t << "</subroutineBody>\n";
  t << "</subroutineDec>\n";
  return t.str();
}
str PropDef(strr cat, strr type, lstr ids) {
  ss t;
  t << "<classVarDec>\n";
  t << "<keyword> " << cat << " </keyword>\n";
  t << type;
  joinComma(t, ids.cbegin(), ids.cend());
  t << "<symbol> ; </symbol>\n";
  t << "</classVarDec>\n";
  return t.str();
}
str Type(strr val) {
  ss t;
  t << "<keyword>" << val << "</keyword>\n";
  return t.str();
}
str LocalDef(strr type, lstr ids) {
  ss t;
  t << "<varDec>\n";
  t << "<keyword> var </keyword>\n";
  t << type;
  joinComma(t, ids.cbegin(), ids.cend());
  t << "<symbol> ; </symbol>\n";
  t << "</varDec>\n";
  return t.str();
}
