import ply.lex as lex
import ply.yacc as yacc

from .nodes import *

"""
list[A] func(){
    A a = new(A);
    list[A] l = new(list[A]);
    l.append(a);  // l owns a
    print(a);  // a is still alive
    return l;  // Pass ownership to caller
}

A func2(){
    A a1 = new(A);
    A a2 = new(A);
    a1.other = a2;  // a1 owns a2
    a2.other = a1;  // COMPILER ERROR: a2 cannot own a1 since a1 already owns a2
    return a1;
}

Dict[A,A] alternative_func2(){
    Dict[A,A] owner_map = new(Dict[A,A]);
    A a1 = new(A);
    A a2 = new(A);
    owner_map[a1] = a2;  // owner_map owns a1 and a2
    owner_map[a2] = a1;
    return owner_map;
}

int main(){
    list[A] l = func();
}
"""

RESERVED = {
    "return": "RETURN",
    "list": "LIST",
}

tokens = (
    "ID", "INT",

    "LPAR", "RPAR", "LBRACE", "RBRACE", "LBRACK", "RBRACK",

    "ASSIGN",

    "SEMICOL",

    "EQ",
) + tuple(RESERVED.values())

# This line is necessary until the version of ply that comes out contains the
# change at:
# https://github.com/dabeaz/ply/commit/cbef61c58f8b1b3b5e2fc3c2414bcac4303538ce
# If this line is not included, the parsetab.py module will regenerate
# every time this is run b/c the signature/hash used to determine if one is
# generated is a string joining of these tokens, whose order can vary based
# on the random ordering of keys in the dictionary of reserved words.
# This line preserves that order.
# This bug still exists as of ply 3.10.
tokens = sorted(tokens)


t_RETURN = "return"
t_LIST = "list"

t_INT = r"\d+"
t_LPAR = r"\("
t_RPAR = r"\)"
t_LBRACE = r"\{"
t_RBRACE = r"\}"
t_LBRACK = r"\["
t_RBRACK = r"\]"
t_ASSIGN = r"=(?!=)"
t_SEMICOL = r";"
t_EQ = r"=="
t_ignore = " \t"


def t_ID(t):
    r"[a-zA-Z_][a-zA-Z0-9_]*"
    t.type = RESERVED.get(t.value, "ID")
    return t


def t_newline(t):
    r"\n+"
    t.lexer.lineno += len(t.value)


def t_error(t):
    if len(t) > 10:
        part = t.value[:10] + "..."
    else:
        part = t.value[:10]
    raise RuntimeError("Unable to handle substring '{}'".format(
        part
    ))


lexer = lex.lex()


#precedence = (,)

def p_translation_unit_empty(p):
    "translation_unit : empty"
    p[0] = TranslationUnit()


def p_translation_unit(p):
    "translation_unit : external_decls"
    p[0] = TranslationUnit(p[1])


def p_external_decls_one(p):
    "external_decls : external_decl"
    p[0] = [p[1]]


def p_external_decls(p):
    "external_decls : external_decls external_decl"
    p[0] = p[1] + [p[2]]


def p_external_decl(p):
    """external_decl : func_def
                     | decl_stmt
    """
    p[0] = p[1]


def p_decl_stmt(p):
    "decl_stmt : decl SEMICOL"
    p[0] = DeclStmt(p[1])


def p_decl(p):
    "decl : type_name ID"
    p[0] = Decl(p[1], p[2])


def p_func_def(p):
    "func_def : type_name ID LPAR args RPAR LBRACE stmts RBRACE"
    p[0] = FuncDef(p[1], p[2], p[4], p[7])


def p_type_name_id(p):
    "type_name : ID"
    p[0] = ID(p[1])


def p_type_name_list(p):
    "type_name : LIST LBRACK type_name RBRACK"
    p[0] = ListType(p[3])


def p_args_empty(p):
    "args : empty"
    p[0] = []


def p_stmts_empty(p):
    "stmts : empty"
    p[0] = []


def p_stmts_one(p):
    "stmts : stmt"
    p[0] = [p[1]]


def p_stmts(p):
    "stmts : stmts stmt"
    p[0] = p[1] + [p[2]]


def p_stmt(p):
    "stmt : decl_stmt"
    p[0] = p[1]


def p_empty(p):
    "empty :"


def p_error(p):
    raise RuntimeError("Syntax error at '{}'".format(p.value))


parser = yacc.yacc()
