from .utils import SlottedClass, optional


INDENT_SIZE = 4


class Node(SlottedClass):
    def lines(self):
        raise NotImplementedError(
            "lines() not implemented for Node '{}'".format(self.__cls__.__name__))

    def __str__(self):
        return "\n".join(self.lines())


class SimpleNode(Node):
    def line(self):
        raise NotImplementedError(
            "line() not implemented for SimpleNode '{}'".format(self.__cls__.__name__))

    def lines(self):
        yield self.line()


class ExternalDecl(Node):
    pass


class Decl(ExternalDecl):
    # TODO
    pass


class DeclSpecifier(SimpleNode):
    pass


class StorageClassSpecifier(DeclSpecifier):
    def specifier(self):
        raise NotImplementedError(
            "specifier() not implemented for StorageClassSpecifier '{}'"
            .format(self.__cls__.__name__))

    def line(self):
        return self.specifier()


class Typedef(StorageClassSpecifier):
    def specifier(self):
        return "typedef"


class Extern(StorageClassSpecifier):
    def specifier(self):
        return "extern"


class Static(StorageClassSpecifier):
    def specifier(self):
        return "static"


class Auto(StorageClassSpecifier):
    def specifier(self):
        return "auto"


class Register(StorageClassSpecifier):
    def specifier(self):
        return "register"


class SpecifierQualifier(SimpleNode):
    pass


class TypeSpecifier(DeclSpecifier, SpecifierQualifier):
    pass


class SimpleTypeSpecifier(TypeSpecifier):
    __attrs__ = ("id",)
    __types__ = {"id": str}

    def line(self):
        return self.id


class StructTypeSpecifier(TypeSpecifier):
    __attrs__ = ("id", )


class UnionTypeSpecifier(TypeSpecifier):
    # TODO
    pass


class EnumSpecifier(TypeSpecifier):
    # TODO
    pass


class TypeQualifier(DeclSpecifier, SpecifierQualifier):
    def qualifier(self):
        raise NotImplementedError(
            "qualifier() not implemented for TypeQualifier '{}'"
            .format(self.__cls__.__name__))


class Const(TypeQualifier):
    def qualifier(self):
        return "const"


class Volatile(TypeQualifier):
    def qualifier(self):
        return "volatile"


class Pointer(SimpleNode):
    # TODO
    pass


class DirectDeclarator(SimpleNode):
    pass


class ConstExpr(SimpleNode):
    # TODO
    pass


class Declarator(SimpleNode):
    # https://msdn.microsoft.com/en-us/library/e5ace6tf.aspx
    __attrs__ = ("direct_declarator", "ptr")
    __types__ = {
        "ptr": optional(Pointer),
        "direct_declarator": DirectDeclarator,
    }
    __defaults__ = {
        "ptr": None,
    }

    def line(self):
        if self.ptr:
            return ptr.line() + self.direct_declarator.line()
        else:
            return self.direct_declarator.line()


class ParamDecl(SimpleNode):
    __attrs__ = ("decl_specifiers", "declarator")
    __types__ = {
        "decl_specifiers": [DeclSpecifier],
        "declarator": optional(Declarator),
    }
    __defaults__ = {
        "decl_specifiers": [],
        "declarator": None,
    }

    def line(self):
        if self.declarator:
            return (" ".join(s.line() for s in self.decl_specifiers) + " " +
                   self.declarator.line())
        else:
            return " ".join(s.line() for s in self.decl_specifiers)


class AbstractParamDecl(SimpleNode):
    # TODO
    # https://msdn.microsoft.com/en-us/library/b198y5xs.aspx
    pass


class ParamTypeList(SimpleNode):
    __attrs__ = ("param_decls", "has_varargs")
    __types__ = {
        "param_decls": [(ParamDecl, AbstractParamDecl)],
        "has_varargs": bool,
    }
    __defaults__ = {
        "has_varargs": False,
    }

    def __init__(self, *args, **kwargs):
        if self.has_varargs:
            assert self.param_decls
        super().__init__(*args, **kwargs)

    def line(self):
        if self.has_varargs:
            return ", ".join(d.line() for d in self.param_decls) + ", ..."
        else:
            return ", ".join(d.line() for d in self.param_decls)


class IDList(SimpleNode):
    __attrs__ = ("ids",)
    __types__ = {"ids": [str]}
    __defaults__ = {"ids": []}

    def line(self):
        return ", ".join(self.ids)


class IDDeclarator(DirectDeclarator):
    __attrs__ = ("id",)
    __types__ = {"id": str}

    def line(self):
        return self.id


class ArrayDeclarator(DirectDeclarator):
    __attrs__ = ("direct_declarator", "size")
    __types__ = {
        "direct_declarator": DirectDeclarator,
        "size": optional(ConstExpr),
    }
    __defaults__ = {
        "size": None,
    }

    def line(self):
        if self.size:
            return self.direct_declarator.line() + "[" + self.size.line() + "]"
        else:
            return self.direct_declarator.line() + "[]"


class FunctionDeclarator(DirectDeclarator):
    __attrs__ = ("direct_declarator", "arg_list")
    __types__ = {
        "direct_declarator": DirectDeclarator,
        "arg_list": optional((ParamTypeList, IDList)),
    }
    __defaults__ = {"arg_list": None}

    def line(self):
        if self.arg_list:
            return "{}({})".format(self.direct_declarator.line(),
                                   ", ".join(a.line() for a in self.arg_list))
        else:
            return "{}()".format(self.direct_declarator.line())


class Stmt(Node):
    pass


class SimpleStmt(Stmt, SimpleNode):
    pass


class AbstractDeclarator(SimpleNode):
    # TODO
    pass


class TypeName(SimpleNode):
    __attrs__ = ("specifier_qualifier_list", "abstract_declarator")
    __types__ = {
        "specifier_qualifier_list": [SpecifierQualifier],
        "abstract_declarator": optional(AbstractDeclarator),
    }
    __defaults__ = {"abstract_declarator": None}

    def line(self):
        line = " ".join(q.line() for q in self.specifier_qualifier_list)
        if self.abstract_declarator:
            line += " " + self.abstract_declarator.line()
        return line


class Expr(SimpleNode):
    def precedence(self):
        raise NotImplementedError

    def scoped_line(self, other):
        if self.precedence() < other.precedence():
            return "(" + other.line() + ")"
        else:
            return other.line()


class AtomicExpr(Expr):
    def precedence(self):
        return 0


class ID(AtomicExpr):
    __attrs__ = ("id",)
    __types__ = {"id": str}

    def line(self):
        return self.id


class Str(AtomicExpr):
    __attrs__ = ("s",)
    __types__ = {"s": str}

    def line(self):
        return '"{}"'.format(self.s)


class Char(AtomicExpr):
    __attrs__ = ("c",)
    __types__ = {"c": str}

    def __init__(self, *args, **kwargs):
        assert len(self.c) == 1
        super().__init__(*args, **kwargs)

    def line(self):
        return "'{}'".format(self.c)


class Int(AtomicExpr):
    __attrs__ = ("n",)
    __types__ = {"n": int}

    def line(self):
        return str(self.n)


class Float(AtomicExpr):
    __attrs__ = ("n",)
    __types__ = {"n": float}

    def line(self):
        return str(self.n)


class Prec1Expr(Expr):
    def precedence(self):
        return 1


class PostInc(Prec1Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return self.scoped_line(self.expr) + "++"


class PostDec(Prec1Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return self.scoped_line(self.expr) + "--"


class Call(Prec1Expr):
    __attrs__ = ("func", "args")
    __types__ = {
        "func": Expr,
        "args": [Expr],
    }
    __defaults__ = {"args": []}

    def line(self):
        return "{}({})".format(
            self.scoped_line(self.func), ", ".join(a.line() for a in self.args))


class Subscript(Prec1Expr):
    __attrs__ = ("expr", "idx")
    __types__ = {
        "expr": Expr,
        "idx": Expr,
    }

    def line(self):
        return "{}[{}]".format(self.scoped_line(self.expr), self.idx.line())


class MemberAccess(Prec1Expr):
    __attrs__ = ("expr", "member")
    __types__ = {
        "expr": Expr,
        "member": str,
    }

    def line(self):
        return "{}.{}".format(self.scoped_line(self.expr), self.member)


class PtrMemberAccess(Prec1Expr):
    __attrs__ = ("expr", "member")
    __types__ = {
        "expr": Expr,
        "member": str,
    }

    def line(self):
        return "{}->{}".format(self.scoped_line(self.expr), self.member)


class Prec2Expr(Expr):
    def precedence(self):
        return 2


class PreInc(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "++{}".format(self.scoped_line(self.expr))


class PreDec(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "--{}".format(self.scoped_line(self.expr))


class UPlus(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "+{}".format(self.scoped_line(self.expr))


class UMinus(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "-{}".format(self.scoped_line(self.expr))


class LogicalNot(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "!{}".format(self.scoped_line(self.expr))


class BitNot(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "~{}".format(self.scoped_line(self.expr))


class Cast(Prec2Expr):
    __attrs__ = ("expr", "type")
    __types__ = {
        "expr": Expr,
        "type": TypeName,
    }

    def line(self):
        return "({}){}".format(self.type.line(), self.scoped_line(self.expr))


class Deref(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "*{}".format(self.scoped_line(self.expr))


class AddrOf(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "&{}".format(self.scoped_line(self.expr))


class SizeOf(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "sizeof({})".format(self.expr.line())


class BinaryExpr(Expr):
    __attrs__ = ("lhs", "rhs")
    __types__ = {
        "lhs": Expr,
        "rhs": Expr,
    }

    def op(self):
        raise NotImplementedError

    def line(self):
        return "{} {} {}".format(self.scoped_line(self.lhs),
                                 self.op(),
                                 self.scoped_line(self.rhs))


class Prec3Expr(Expr):
    def precedence(self):
        return 3


class Mul(Prec3Expr, BinaryExpr):
    def op(self):
        return "*"


class Div(Prec3Expr, BinaryExpr):
    def op(self):
        return "/"


class Mod(Prec3Expr, BinaryExpr):
    def op(self):
        return "%"


class Prec4Expr(Expr):
    def precedence(self):
        return 4


class Add(Prec4Expr, BinaryExpr):
    def op(self):
        return "+"


class Sub(Prec4Expr, BinaryExpr):
    def op(self):
        return "-"


class Prec5Expr(Expr):
    def precedence(self):
        return 5


class LShift(Prec5Expr, BinaryExpr):
    def op(self):
        return "<<"


class RShift(Prec5Expr, BinaryExpr):
    def op(self):
        return ">>"


class Prec6Expr(Expr):
    def precedence(self):
        return 6


class Lt(Prec6Expr, BinaryExpr):
    def op(self):
        return "<"


class Lte(Prec6Expr, BinaryExpr):
    def op(self):
        return "<="


class Gt(Prec6Expr, BinaryExpr):
    def op(self):
        return ">"


class Gte(Prec6Expr, BinaryExpr):
    def op(self):
        return ">="


class Prec7Expr(Expr):
    def precedence(self):
        return 7


class Eq(Prec7Expr, BinaryExpr):
    def op(self):
        return "=="


class Ne(Prec7Expr, BinaryExpr):
    def op(self):
        return "!="


class Prec8Expr(Expr):
    def precedence(self):
        return 8


class BitAnd(Prec8Expr, BinaryExpr):
    def op(self):
        return "&"


class Prec9Expr(Expr):
    def precedence(self):
        return 9


class BitXor(Prec9Expr, BinaryExpr):
    def op(self):
        return "^"


class Prec10Expr(Expr):
    def precedence(self):
        return 10


class BitOr(Prec10Expr, BinaryExpr):
    def op(self):
        return "|"


class Prec11Expr(Expr):
    def precedence(self):
        return 11


class And(Prec11Expr, BinaryExpr):
    def op(self):
        return "&&"


class Prec12Expr(Expr):
    def precedence(self):
        return 12


class Or(Prec12Expr, BinaryExpr):
    def op(self):
        return "||"


class Prec13Expr(Expr):
    def precedence(self):
        return 13


class TernaryCondExpr(Prec13Expr):
    __attrs__ = ("cond", "expr1", "expr2")

    def line(self):
        return "{} ? {} : {}".format(self.scoped_line(self.cond),
                                     self.scoped_line(self.expr1),
                                     self.scoped_line(self.expr2))


class Prec14Expr(Expr):
    def precedence(self):
        return 14


class AssignmentExpr(Prec14Expr, BinaryExpr):
    pass


class SimpleAssignment(AssignmentExpr):
    def op(self):
        return "="


class IAdd(AssignmentExpr):
    def op(self):
        return "+="


class ISub(AssignmentExpr):
    def op(self):
        return "-="


class IMul(AssignmentExpr):
    def op(self):
        return "*="


class IDiv(AssignmentExpr):
    def op(self):
        return "/="


class IMod(AssignmentExpr):
    def op(self):
        return "%="


class ILShift(AssignmentExpr):
    def op(self):
        return "<<="


class IRShift(AssignmentExpr):
    def op(self):
        return ">>="


class IBitAnd(AssignmentExpr):
    def op(self):
        return "&="


class IBitOr(AssignmentExpr):
    def op(self):
        return "|="


class IBitXor(AssignmentExpr):
    def op(self):
        return "^="


class InitializerList(Expr):
    __attrs__ = ("initializers",)
    __types__ = {"initializers": [Expr]}

    def line(self):
        return "{" + ", ".format(i.line() for i in self.initializers) + "}"


class InitDeclarator(SimpleStmt):
    __attrs__ = ("declarator", "initializer")
    __types__ = {
        "declarator": Declarator,
        "initializer": optional(Expr),
    }
    __defaults__ = {
        "initializer": None,
    }

    def line(self):
        if self.initializer:
            return self.declarator.line() + " = " + self.initializer.line()
        else:
            return self.declarator.line()


class ExprStmt(SimpleStmt):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "{};".format(self.expr.line())


class Declaration(SimpleStmt):
    __attrs__= ("decl_specifiers", "init_declarator_list")
    __types__ = {
        "decl_specifiers": [DeclSpecifier],
        "init_declarator_list": optional([InitDeclarator]),
    }
    __defaults__ = {
        "init_declarator_list": None,
    }

    def line(self):
        decl_spec_line = " ".join(d.line() for d in self.decl_specifiers)
        if self.init_declarator_list:
            init_declarator_line = ", ".join(d.line() for d in self.init_declarator_list)
            return decl_spec_line + " " + init_declarator_line + ";"
        else:
            return decl_spec_line + ";"


class FunctionDef(ExternalDecl):
    __attrs__ = ("decl_specifiers", "declarator", "stmts")
    __types__ = {
        "declarator": Declarator,
        "stmts": [Stmt],
        "decl_specifiers": [DeclSpecifier],
    }
    __defaults__ = {
        "decl_specifiers": [],
        "stmts": [],
    }

    def lines(self):
        if self.decl_specifiers:
            decl_spec_line = " ".join(d.line() for d in self.decl_specifiers)
            line1 = decl_spec_line + " "
        else:
            line1 = ""
        line1 += self.declarator.line() + "{"

        if self.stmts:
            yield line1

            for stmt in self.stmts:
                for line in stmt.lines():
                    yield INDENT_SIZE * " " + line

            yield "}"
        else:
            yield line1 + "}"


class Macro(Node):
    pass


class SimpleMacro(Macro, SimpleNode):
    pass


class Include(SimpleMacro):
    __attrs__ = ("path", "is_system")
    __types__ = {
        "path": str,
        "is_system": bool,
    }
    __defaults__ = {"is_system": False}

    def line(self):
        if self.is_system:
            return "#include <{}>".format(self.path)
        else:
            return "#include \"{}\"".format(self.path)


class TranslationUnit(Node):
    __attrs__ = ("external_decls",)
    __types__ = {"external_decls": [(ExternalDecl, Macro)]}
    __defaults__ = {"external_decls": []}

    def lines(self):
        for decl in self.external_decls:
            yield from decl.lines()
