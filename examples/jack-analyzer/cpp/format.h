#include <string>
#include <utility>
#include <list>

using str = std::string;
using strr = const str&;
using lst = std::list<str>;
using lstr = const lst&;
using lpair = std::pair<lst, lst>;
using lpairr = const lpair&;
using ss = std::stringstream;
using ssr = const ss&;

lst prepend(lst, strr);
str escape(strr);

str ArrayExpr(strr, strr);
str BinOp(strr, strr, strr);
str Call(lstr, lstr);
str IntConst(strr);
str Term(strr);
str Parens(strr);
str Neg(strr);
str Not(strr);
str PrimVal(strr);
str StrConst(strr);
str Identifier(strr);
str Body(lstr, lstr);
str DoStmt(strr);
str IfStmt(strr, lstr, lstr);
str LetStmt(strr, strr);
str ReturnStmt(strr);
str WhileStmt(strr, lstr);
str Class(strr, lpairr);
str LocalDef(strr, lstr);
str MethodDef(strr, strr, strr, lstr, strr);
str PropDef(strr, strr, lstr);
str Type(strr);
