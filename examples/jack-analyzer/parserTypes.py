def join(list):
    return "".join(map(str,list))

def joinComma(list):
    return f"\n<symbol> , </symbol>\n".join(map(str,list))

def joinDot(list):
    return f"\n<symbol> . </symbol>\n".join(map(str,list))

class Class:
    def __init__(self, name, defs):
        self.name = name
        self.fields = defs[0]
        self.methods = defs[1]

    def __str__(self):
        return f"""
<class>
<keyword> class </keyword>
{self.name}
<symbol> {{ </symbol>
{join(self.fields)}
{join(self.methods)}
<symbol> }} </symbol>
</class>
"""

class Expr:
    def __init__(self, what):
        self.what = what

    def __str__(self):
        return f"""
<expression>
{self.what}
</expression>
"""

class Term:
    def __init__(self, what):
        self.what = what

    def __str__(self):
        return f"""
<term>
{self.what}
</term>
"""

class PropDef:
    def __init__(self, cat, type, identifiers):
        self.cat = cat
        self.type = type
        self.identifiers = identifiers

    def __str__(self):
        return f"""
<classVarDec>
<keyword> {self.cat} </keyword>
{self.type}
{joinComma(self.identifiers)}
<symbol> ; </symbol>
</classVarDec>
"""


class MethodDef:
    def __init__(self, cat, type, name, params, body):
        self.cat=cat
        self.type=type
        self.name=name
        self.params=params
        self.body=body

    def __str__(self):
        return f"""
<subroutineDec>
<keyword> {self.cat} </keyword>
{self.type}
{self.name}
<symbol> ( </symbol>
<parameterList>
{joinComma(self.params)}
</parameterList>
<symbol> ) </symbol>
<subroutineBody>
<symbol> {{ </symbol>
{self.body}
<symbol> }} </symbol>
</subroutineBody>
</subroutineDec>
"""

class Body:
    def __init__(self, local, statements):
        self.local = local
        self.statements = statements

    def __str__(self):
        return f"""
{join(self.local)}
<statements>
{join(self.statements)}
</statements>
"""

class LocalDef:
    def __init__(self, type, identifiers):
        self.type = type
        self.identifiers = identifiers

    def __str__(self):
        return f"""
<varDec>
<keyword> var </keyword>
{self.type}
{joinComma(self.identifiers)}
<symbol> ; </symbol>
</varDec>
"""

class Identifier:
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return f"<identifier> {self.name} </identifier>"

class PrimType:
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return f"<keyword> {self.name} </keyword>"

class LetStmt:
    def __init__(self,var,expr):
        self.var=var
        self.expr=expr

    def __str__(self):
        return f"""
<letStatement>
<keyword> let </keyword>
{self.var}
<symbol> = </symbol>
<expression>
{self.expr}
</expression>
<symbol> ; </symbol>
</letStatement>
"""

class DoStmt:
    def __init__(self,call):
        self.call=call

    def __str__(self):
        return f"""
<doStatement>
<keyword> do </keyword>
{self.call}
<symbol> ; </symbol>
</doStatement>
"""

class ReturnStmt:
    def __init__(self,expr=None):
        self.expr=expr

    def __str__(self):
        return f"""
<returnStatement>
<keyword> return </keyword>
{Expr(self.expr) if self.expr else ""}
<symbol> ; </symbol>
</returnStatement>
"""

class WhileStmt:
    def __init__(self,cond,body):
        self.cond=cond
        self.body=body

    def __str__(self):
        return f"""
<whileStatement>
<keyword> while </keyword>
<symbol> ( </symbol>
<expression>
{self.cond}
</expression>
<symbol> ) </symbol>
<symbol> {{ </symbol>
<statements>
{join(self.body)}
</statements>
<symbol> }} </symbol>
</whileStatement>
"""

class IfStmt:
    def __init__(self,cond,iftrue,iffalse):
        self.cond=cond
        self.iftrue=iftrue
        self.iffalse=iffalse

    def _iffalse(self):
        if self.iffalse is None:
            return ""
        else:
            return f"""
<keyword> else </keyword>
<symbol> {{ </symbol>
<statements>
{join(self.iffalse)}
</statements>
<symbol> }} </symbol>
"""

    def __str__(self):
        return f"""
<ifStatement>
<keyword> if </keyword>
<symbol> ( </symbol>
<expression>
{self.cond}
</expression>
<symbol> ) </symbol>
<symbol> {{ </symbol>
<statements>
{join(self.iftrue)}
</statements>
<symbol> }} </symbol>
{self._iffalse()}
</ifStatement>
"""

class ArrayExpr:
    def __init__(self, name, idx):
        self.name=name
        self.idx=idx

    def __str__(self):
        return f"""
{self.name}
<symbol> [ </symbol>
<expression>
{self.idx}
</expression>
<symbol> ] </symbol>
"""

class IntConst:
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return f"<integerConstant>{self.value}</integerConstant>"

class StrConst:
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return f"<stringConstant>{self.value}</stringConstant>"

class Call:
    def __init__(self, method, arguments):
        self.method = method
        self.arguments = arguments

    def __str__(self):
        return f"""
{joinDot(self.method)}
<symbol> ( </symbol>
<expressionList>
{joinComma(map(Expr, self.arguments))}
</expressionList>
<symbol> ) </symbol>
"""

class PrimVal:
    def __init__(self, val):
        self.val = val

    def __str__(self):
        return f"<keyword> {self.val} </keyword>"

class Not:
    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return f"""
<term>
<symbol> ~ </symbol>
{self.expr}
</term>
"""

class Neg:
    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return f"""
<term>
<symbol> - </symbol>
{self.expr}
</term>
"""


class Parens:
    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return f"""
<term>
<symbol> ( </symbol>
<expression>
{self.expr}
</expression>
<symbol> ) </symbol>
</term>
"""

class BinOp:
    def __init__(self, op, a, b):
        self.op = op
        self.a = a
        self.b = b

    def __str__(self):
        return f"""
{self.a}
<symbol> {escape(self.op)} </symbol>
{self.b}
"""

def escape(s):
    return s.replace('&', '&amp;')\
        .replace('"', '&quot;')\
        .replace('<', '&lt;')\
        .replace('>', '&gt;')

class Parameter:
    def __init__(self,type, name):
        self.type=type
        self.name=name

    def __str__(self):
        return f"""
{self.type}
{self.name}
"""
