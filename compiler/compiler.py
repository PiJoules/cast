import ply.lex as lex
import ply.yacc as yacc

from .cast.utils import SlottedClass, optional
from .nodes import *
from .cast import nodes as cnodes

"""
List[A] func(){
    A a = new(A);
    List[A] l = new(List[A]);
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
    List[A] l = func();
}
"""

RESERVED = {
    "return": "RETURN",
    "List": "LIST",
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
t_LIST = "List"

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


def t_comment(t):
    r"[ ]*\#[^\n]*"


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
            | int_literal
    """
    p[0] = p[1]


def p_int_literal(p):
    "int_literal : INT"
    p[0] = Int(int(p[1]))


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


class TypeSignature(SlottedClass):
    pass


class FuncType(TypeSignature):
    __attrs__ = ("return_type", "arg_types")
    __types__ = {
        "return_type": TypeSignature,
        "arg_types": [TypeSignature]
    }
    __defaults__ = {"arg_types": []}

    def __hash__(self):
        return (hash("FuncType") ^ hash(frozenset(self.arg_types)) ^
                hash(self.return_type))

    def __eq__(self, other):
        return super().__eq__(other)

    def __str__(self):
        return "{}({})".format(
            self.return_type,
            ", ".join(str(a) for a in self.arg_types)
        )


class SimpleType(TypeSignature):
    __attrs__ = ("id",)
    __types__ = {"id": str}

    def __hash__(self):
        return hash(self.id)

    def __eq__(self, other):
        return super().__eq__(other)

    def __str__(self):
        return self.id


class ListTypeSig(TypeSignature):
    __attrs__ = ("contents",)
    __types__ = {"contents": TypeSignature}

    def __hash__(self):
        return hash("List") ^ hash(self.contents)

    def __eq__(self, other):
        return super().__eq__(other)

    def __str__(self):
        return "List[{}]".format(self.contents)


class GenericType(TypeSignature):
    def __hash__(self):
        return hash("generic")

    def __eq__(self, other):
        return super().__eq__(other)

    def __str__(self):
        return "<generic>"


GENERIC_TYPE = GenericType()
VOID_TYPE = SimpleType("void")
CHAR_TYPE = SimpleType("char")
INT_TYPE = SimpleType("int")


BASE_LANG_TYPES = {
    VOID_TYPE,
    CHAR_TYPE,
    INT_TYPE,
}


C_TYPE_MAPPING = {
    "int": "Integer",
}


BASE_LANG_VARS = {
    "print": FuncType(VOID_TYPE, [GENERIC_TYPE]),
}


class Compiler:
    def __init__(self):
        self.__code = None
        self.__lang_ast = None
        self.__c_ast = None
        self.__call_stack = []

        self.__type_frames = [set(BASE_LANG_TYPES)]
        self.__var_frames = [dict(BASE_LANG_VARS)]
        self.__objs_owned_by_frames = [set()]

        self.__pending_includes = set()
        self.__generated_files = set()

    def compile(self, code):
        module = self.__lang_ast = parser.parse(code)
        self.__c_ast = self.visit(module)
        assert not self.__call_stack

    def type_frame(self):
        return self.__type_frames[-1]

    def var_frame(self):
        return self.__var_frames[-1]

    def objs_owned_by_frame(self):
        return self.__objs_owned_by_frames[-1]

    def enter_scope(self):
        new_type_scope = set(self.type_frame())
        new_var_scope = dict(self.var_frame())
        self.__type_frames.append(new_type_scope)
        self.__var_frames.append(new_var_scope)
        self.__objs_owned_by_frames.append(set())

    def exit_scope(self):
        self.__type_frames.pop()
        self.__var_frames.pop()
        self.__objs_owned_by_frames.pop()
        assert self.__type_frames
        assert self.__var_frames
        assert self.__objs_owned_by_frames

    def record_var_type(self, varname, type_sig):
        self.assert_type_sig_is_known(type_sig)

        if not isinstance(type_sig, FuncType):
            self.objs_owned_by_frame().add(varname)

        if varname in self.var_frame():
            raise RuntimeError(
                "'{}' was already declared in this scope as '{}'"
                .format(varname, self.var_frame()[varname])
            )
        self.var_frame()[varname] = type_sig

    def lang_ast(self):
        return self.__lang_ast

    def c_ast(self):
        return self.__c_ast

    def generated_files(self):
        return self.__generated_files

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

    def mangled_ctor(self, mangled_name):
        return "new_{}".format(mangled_name)

    def convert_c_primitive_type(self, id):
        return C_TYPE_MAPPING.get(id, id)

    def type_signature_from_typename(self, typename):
        if isinstance(typename, ListType):
            return ListTypeSig(self.type_signature_from_typename(typename.contents))
        elif isinstance(typename, ID):
            return SimpleType(typename.id)
        else:
            raise NotImplementedError(
                "Unable to convert typename '{}' to type signature"
                .format(typename.__class__.__name__)
            )

    def mangled_c_type_from_type_sig(self, type_sig):
        if isinstance(type_sig, ListTypeSig):
            return "List<{}>".format(self.mangled_c_type_from_type_sig(type_sig.contents) + "*")
        elif isinstance(type_sig, SimpleType):
            return self.convert_c_primitive_type(type_sig.id)
        else:
            raise NotImplementedError(
                "Unable to convert type signature '{}' to mangleed c type"
                .format(type_sig.__class__.__name__)
            )

    def type_spec_and_decltor_from_type_signature_and_varname(self, sig, name):
        if isinstance(sig, ListType):
            self.try_to_make_list_of(sig.contents)
            type_specs = [cnodes.Type(mangled_name)]
            declarators = [cnodes.ID(varname)]
            return type_specs, declarators
        elif isinstance(sig, SimpleType):
            raise NotImplementedError
        else:
            raise RuntimeError(
                "Unable to handle type signature '{}'"
                .format(sig.__class__.__name__)
            )

    def type_info_from_type_sig_and_name(self, type_sig, varname):
        mangled_name = self.mangled_c_type_from_type_sig(type_sig)
        if isinstance(type_sig, ListTypeSig):
            type_specs = [cnodes.Type(mangled_name)]
            declarators = [cnodes.ID(varname)]
        elif isinstance(type_sig, SimpleType):
            type_specs = [cnodes.Type(mangled_name)]
            declarators = [cnodes.ID(varname)]
        else:
            raise NotImplementedError("Unable to handle type signature '{}'".format(
                type_sig.__class__.__name__
            ))
        return type_specs, declarators

    def assert_type_sig_is_known(self, type_sig):
        if isinstance(type_sig, ListTypeSig):
            self.assert_type_sig_is_known(type_sig.contents)
        elif isinstance(type_sig, FuncType):
            self.assert_type_sig_is_known(type_sig.return_type)
            for arg_type in type_sig.arg_types:
                self.assert_type_sig_is_known(arg_type)
        elif isinstance(type_sig, SimpleType):
            if type_sig not in self.type_frame():
                raise RuntimeError("Unknown type '{}'".format(type_sig))
        else:
            raise RuntimeError("Unknown type signature '{}'".format(type_sig.__class__.__name__))

    def visit_ID(self, id_node):
        return cnodes.ID(id_node.id)

    def visit_Access(self, access):
        return cnodes.PtrMemberAccess(
            self.visit(access.expr),
            access.member
        )

    def visit_Call(self, call):
        return cnodes.Call(
            self.visit(call.expr),
            [self.visit(a) for a in call.args]
        )

    def visit_ExprStmt(self, expr_stmt):
        return cnodes.ExprStmt(self.visit(expr_stmt.expr))

    def visit_Return(self, return_stmt):
        if return_stmt.expr:
            return cnodes.Return(self.visit(return_stmt.expr))
        else:
            return cnodes.Return()

    def visit_Int(self, int_node):
        return cnodes.New(cnodes.Call(cnodes.ID("Integer"), [cnodes.Int(int_node.n)]))

    def visit_DeclStmt(self, decl_stmt):
        declaration = decl_stmt.decl

        # TODO: Implement for other decls
        assert isinstance(declaration, VarDecl)

        decl_type = declaration.type
        varname = declaration.id
        init_assign_val = declaration.expr

        type_sig = self.type_signature_from_typename(decl_type)
        self.assert_type_sig_is_known(type_sig)
        self.record_var_type(varname, type_sig)

        mangled_name = self.mangled_c_type_from_type_sig(type_sig)

        type_specs, declarators = self.type_info_from_type_sig_and_name(
            type_sig, varname,
        )

        # Can only declare 1 variable at a time
        assert len(declarators) == 1
        declarator = declarators[0]

        if init_assign_val:
            cinit_assign = cnodes.InitAssign(
                cnodes.Pointer(declarator),
                self.visit(init_assign_val)
            )
        else:
            cinit_assign = cnodes.InitAssign(
                cnodes.Pointer(declarator),
                cnodes.New(cnodes.Call(cnodes.ID(mangled_name))),
            )
        return cnodes.Decl(type_specs, [cinit_assign])

    def visit_Assign(self, assign):
        return cnodes.ExprStmt(cnodes.Assign(
            self.visit(assign.assignable),
            self.visit(assign.expr)
        ))

    def visit_FuncDef(self, func_def):
        return_t = func_def.return_type
        func_name = func_def.id
        args = func_def.args
        body = func_def.body

        return_type_sig = self.type_signature_from_typename(return_t)

        # TODO: Do same for args
        self.assert_type_sig_is_known(return_type_sig)

        type_specs, declarators = self.type_info_from_type_sig_and_name(return_type_sig, func_name)
        assert len(declarators) == 1
        assert len(type_specs) == 1

        # TODO: Include args later
        func_sig = FuncType(return_type_sig)

        self.enter_scope()

        self.record_var_type(func_name, func_sig)
        cbody = [self.visit(stmt) for stmt in body]
        objs_owned_by_func = self.objs_owned_by_frame()

        for varname in objs_owned_by_func:
            cbody.append(cnodes.ExprStmt(cnodes.Delete(cnodes.ID(varname))))

        self.exit_scope()

        cfunc_def = cnodes.FuncDef(
            type_specs,
            # TODO: handle args
            cnodes.Pointer(cnodes.FuncDeclarator(declarators[0])),
            cbody
        )
        return cfunc_def

    def visit_TranslationUnit(self, tu):
        includes = [cnodes.Include("lang.h")]
        stmts = [self.visit(d) for d in tu.external_decls]

        stmts.append(cnodes.FuncDef(
            [cnodes.Type("int")],
            cnodes.FuncDeclarator(cnodes.ID("main")),
            [cnodes.ExprStmt(cnodes.Call(cnodes.ID("start")))]
        ))

        includes += [cnodes.Include(i) for i in self.__pending_includes]
        stmts = includes + stmts
        return cnodes.TranslationUnit(stmts)
