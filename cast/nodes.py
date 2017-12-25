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


class TypeSpecifier(DeclSpecifier):
    pass


class SimpleTypeSpecifier(TypeSpecifier):
    __attrs__ = ("id",)
    __types__ = {"id": str}

    def line(self):
        return self.id


class StructTypeSpecifier(TypeSpecifier):
    # TODO
    pass


class UnionTypeSpecifier(TypeSpecifier):
    # TODO
    pass


class EnumSpecifier(TypeSpecifier):
    # TODO
    pass


class TypeQualifier(DeclSpecifier):
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
    __attrs__ = ("ptr", "direct_declarator")
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


class Expr(SimpleNode):
    def precedence(self):
        raise NotImplementedError

    def scoped_line(self, other):
        if self.prec() < other.prec():
            return "(" + other.line() + ")"
        else:
            return other.line()


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
            self.func.line(), ", ".join(a.line() for a in self.args))


class Subscript(Prec1Expr):
    __attrs__ = ("expr", "idx")
    __types__ = {
        "expr": Expr,
        "idx": Expr,
    }

    def line(self):
        return "{}[{}]".format(self.expr.line(), self.idx.line())


class MemberAccess(Prec1Expr):
    __attrs__ = ("expr", "member")
    __types__ = {
        "expr": Expr,
        "member": str,
    }

    def line(self):
        return "{}.{}".format(self.expr.line(), self.member)


class PtrMemberAccess(Prec1Expr):
    __attrs__ = ("expr", "member")
    __types__ = {
        "expr": Expr,
        "member": str,
    }

    def line(self):
        return "{}->{}".format(self.expr.line(), self.member)


class Prec2Expr(Expr):
    pass


class PreInc(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "++{}".format(self.expr.line())


class PreDec(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "(--{})".format(self.expr.line())


class UPlus(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}


class BinaryExpr(Expr):
    # TODO
    pass


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
            return decl_specifiers + " " + init_declarator_line + ";"
        else:
            return decl_specifiers + ";"


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
        yield line1

        for line in self.stmts:
            yield INDENT_SIZE * " " + line

        yield "}"


class TranslationUnit(Node):
    __attrs__ = ("external_decls",)
    __types__ = {"external_decls": [ExternalDecl]}
    __defaults__ = {"external_decls": []}

    def lines(self):
        for decl in self.external_decls:
            yield from decl.lines()
