import ply.lex as lex
import ply.yacc as yacc

from .nodes import *
from .cast import nodes as cnodes

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

    "SEMICOL", "DOT",

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
t_DOT = r"\."
t_EQ = r"=="
t_ignore = " \t"


def t_ID(t):
    r"[a-zA-Z_][a-zA-Z0-9_]*"
    t.type = RESERVED.get(t.value, "ID")
    return t


def t_newline(t):
    r"\n+"
    t.lexer.lineno += len(t.value)


def col(tok):
    last_cr = tok.lexer.lexdata.rfind("\n", 0, tok.lexpos)
    if last_cr < 0:
        last_cr = 0
    return tok.lexpos - last_cr


def t_error(t):
    if len(t.value) > 10:
        part = t.value[:10] + "..."
    else:
        part = t.value[:10]
    lineno = t.lineno
    colno = col(t)

    raise RuntimeError(
        "Unable to match substring '{}' at line {}, col {} against any token".format(
            part, lineno, colno
        ))


lexer = lex.lex()


precedence = (
    (
        "left",
        "DOT",      # Member access
        "LPAR",     # Call
    ),
)


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
    p[0] = VarDecl(p[1], p[2])


def p_decl_with_init(p):
    "decl : type_name ID ASSIGN expr"
    p[0] = VarDecl(p[1], p[2], p[4])


def p_func_def(p):
    "func_def : type_name ID LPAR params RPAR LBRACE stmts RBRACE"
    p[0] = FuncDef(p[1], p[2], p[4], p[7])


def p_type_name_id(p):
    "type_name : ID"
    p[0] = ID(p[1])


def p_type_name_list(p):
    "type_name : LIST LBRACK type_name RBRACK"
    p[0] = ListType(p[3])


def p_params_empty(p):
    "params : empty"
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
    """stmt : decl_stmt
            | expr_stmt
            | return_stmt
            | assign
    """
    p[0] = p[1]


def p_assign(p):
    "assign : assignable ASSIGN expr SEMICOL"
    p[0] = Assign(p[1], p[3])


def p_assignable(p):
    """assignable : id_expr"""
    p[0] = p[1]


def p_return_stmt_no_val(p):
    "return_stmt : RETURN SEMICOL"
    p[0] = Return()


def p_return_stmt(p):
    "return_stmt : RETURN expr SEMICOL"
    p[0] = Return(p[2])


def p_expr_stmt(p):
    "expr_stmt : expr SEMICOL"
    p[0] = ExprStmt(p[1])


def p_expr(p):
    """expr : call
            | member_access
            | id_expr
    """
    p[0] = p[1]


def p_member_access(p):
    "member_access : expr DOT ID"
    p[0] = Access(p[1], p[3])


def p_expr_id(p):
    "id_expr : ID"
    p[0] = ID(p[1])


def p_call(p):
    "call : expr LPAR exprs RPAR"
    p[0] = Call(p[1], p[3])


def p_exprs_empty(p):
    "exprs : empty"
    p[0] = []


def p_exprs_one(p):
    "exprs : expr"
    p[0] = [p[1]]


def p_exprs(p):
    "exprs : exprs expr"
    p[0] = p[1] + [p[2]]


def p_empty(p):
    "empty :"


def p_error(p):
    lineno = p.lineno
    colno = col(p)
    raise RuntimeError(
        "Syntax error '{}' at line {}, col {}".format(
            p.value, lineno, colno
        ))


parser = yacc.yacc()


class Compiler:
    def __init__(self):
        self.__code = None
        self.__lang_ast = None
        self.__c_ast = None
        self.__call_stack = []

    def compile(self, code):
        module = self.__lang_ast = parser.parse(code)
        self.__c_ast = self.visit(module)

    def lang_ast(self):
        return self.__lang_ast

    def visit(self, node):
        name = node.__class__.__name__
        method_name = "visit_" + name
        self.__call_stack.append(method_name)

        if hasattr(self, method_name):
            method = getattr(self, method_name)
            result = method(node)
            self.__call_stack.pop()
            return result
        else:
            self.__dump_call_stack()
            raise NotImplementedError(
                "No visit method implemented for node '{}'".format(
                    name
                ))

    def __dump_call_stack(self):
        print("------- Visitor Stack -------")
        for func in self.__call_stack:
            print(func)

    def type_info_from_vardecl(self, vardecl):
        varname = vardecl.id
        typename = vardecl.type
        return self.type_info_from_typename_and_id(typename, varname)

    def type_info_from_typename_and_id(self, typename, varname):
        if isinstance(typename, ListType):
            raise NotImplementedError
        elif isinstance(typename, ID):
            type_specs = [cnodes.Type(typename.id)]
            declarators = [cnodes.ID(varname)]
            return type_specs, declarators
        else:
            raise NotImplementedError("Unable to handle typename '{}'".format(
                typename.__class__.__name__
            ))

    def visit_DeclStmt(self, decl_stmt):
        # TODO: Implement for other decls
        assert isinstance(decl_stmt.decl, VarDecl)

        type_specs, declarators = self.type_info_from_vardecl(decl_stmt.decl)

        cdecl = cnodes.Decl(type_specs, declarators)
        return cdecl

    def visit_FuncDef(self, func_def):
        return_t = func_def.return_type
        func_name = func_def.id
        args = func_def.args
        body = func_def.body

        type_specs, declarators = self.type_info_from_typename_and_id(return_t, func_name)
        cbody = [self.visit(stmt) for stmt in body]

        cfunc_def = cnodes.FuncDef(
            type_specs,
            declarators,
            cbody
        )
        return cfunc_def

    def visit_TranslationUnit(self, tu):
        cnodes = [self.visit(d) for d in tu.external_decls]
        return cnodes.TranslationUnit(cnodes)
