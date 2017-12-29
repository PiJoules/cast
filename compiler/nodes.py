from .cast.utils import SlottedClass, optional


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


class Decl(Node):
    pass


class SimpleDecl(Decl, SimpleNode):
    pass


class Stmt(Node):
    pass


class Expr(SimpleNode):
    pass


class ExternalStmt(Stmt):
    pass


class TypeName(SimpleNode):
    pass


class ListType(TypeName):
    __attrs__ = ("contents",)
    __types__ = {"contents": TypeName}

    def line(self):
        return "list[{}]".format(self.contents.line())


class VarDecl(SimpleDecl):
    __attrs__ = ("type", "id", "expr")
    __types__ = {
        "type": TypeName,
        "id": str,
        "expr": optional(Expr),
    }
    __defaults__ = {"expr": None}

    def line(self):
        if not self.expr:
            return "{} {}".format(self.type.line(), self.id)
        else:
            return "{} {} = {}".format(
                self.type.line(), self.id, self.expr.line())


class SimpleStmt(Stmt, SimpleNode):
    pass


class Assign(SimpleStmt):
    __attrs__ = ("assignable", "expr")
    __types__ = {
        "assignable": Expr,
        "expr": Expr,
    }

    def line(self):
        return "{} = {};".format(self.assignable.line(), self.expr.line())


class DeclStmt(SimpleStmt, ExternalStmt):
    __attrs__ = ("decl",)
    __types__ = {"decl": Decl}

    def line(self):
        return self.decl.line() + ";"


class Return(SimpleStmt):
    __attrs__ = ("expr",)
    __types__ = {"expr": optional(Expr)}
    __defaults__ = {"expr": None}

    def line(self):
        if self.expr:
            return "return {};".format(self.expr.line())
        else:
            return "return;"


class ExprStmt(SimpleStmt):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return self.expr.line() + ";"


class ID(TypeName, Expr):
    __attrs__ = ("id",)
    __types__ = {"id": str}

    def line(self):
        return self.id


class Int(Expr):
    __attrs__ = ("n",)
    __types__ = {"n": int}

    def line(self):
        return str(self.n)


class Access(Expr):
    __attrs__ = ("expr", "member")
    __types__ = {
        "expr": Expr,
        "member": str,
    }

    def line(self):
        return "{}.{}".format(self.expr.line(), self.member)


class Call(Expr):
    __attrs__ = ("expr", "args")
    __types__ = {
        "expr": Expr,
        "args": [Expr],
    }
    __defaults__ = {"args": []}

    def line(self):
        return "{}({})".format(
            self.expr.line(),
            ", ".join(a.line() for a in self.args))


class FuncDef(ExternalStmt):
    __attrs__ = ("return_type", "id", "args", "body")
    __types__ = {
        "return_type": TypeName,
        "id": str,
        "args": [VarDecl],
        "body": [Stmt],
    }
    __defaults__ = {
        "args": [],
        "body": [],
    }

    def lines(self):
        line1 = "{} {}({})".format(self.return_type.line(),
                                   self.id,
                                   ", ".join(a.line() for a in self.args))
        yield line1 + "{"
        for stmt in self.body:
            for line in stmt.lines():
                yield INDENT_SIZE * " " + line
        yield "}"


class TranslationUnit(Node):
    __attrs__ = ("external_decls",)
    __types__ = {"external_decls": [ExternalStmt]}
    __defaults__ = {"external_decls": []}

    def lines(self):
        for decl in self.external_decls:
            for line in decl.lines():
                yield line
