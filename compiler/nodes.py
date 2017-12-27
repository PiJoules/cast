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


class Decl(Node):
    # TODO
    pass


class Stmt(Node):
    # TODO
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


class Decl(SimpleNode):
    __attrs__ = ("type", "id")
    __types__ = {
        "type": TypeName,
        "id": str,
    }

    def line(self):
        return "{} {}".format(self.type.line(), self.id)


class SimpleStmt(Stmt, SimpleNode):
    pass


class DeclStmt(SimpleStmt, ExternalStmt):
    __attrs__ = ("decl",)
    __types__ = {"decl": Decl}

    def line(self):
        return self.decl.line() + ";"


class ID(TypeName):
    __attrs__ = ("id",)
    __types__ = {"id": str}

    def line(self):
        return self.id


class FuncDef(ExternalStmt):
    __attrs__ = ("return_type", "id", "args", "body")
    __types__ = {
        "return_type": TypeName,
        "id": str,
        "args": [Decl],
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
