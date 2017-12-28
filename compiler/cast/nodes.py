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
    """Only stuff allowed outside of a function."""


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


class Type(TypeSpecifier):
    __attrs__ = ("id",)
    __types__ = {"id": str}

    def line(self):
        return self.id


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


class Declarator(SimpleNode):
    """The part of the declaration that specifies the name of the variable (if
    not abstract), if the type is an array, or if the type is a function.

    https://msdn.microsoft.com/en-us/library/e5ace6tf.aspx
    """


class Expr(SimpleNode):
    def precedence(self):
        raise NotImplementedError

    def scoped_line(self, other):
        if self.precedence() < other.precedence():
            return "(" + other.line() + ")"
        else:
            return other.line()


class Pointer(Declarator):
    __attrs__ = ("declarator", "type_qualifiers")
    __types__ = {
        "type_qualifiers": [TypeQualifier],
        "declarator": optional(Declarator),
    }
    __defaults__ = {
        "type_qualifiers": [],
        "declarator": None,
    }

    def line(self):
        line = "*"
        if self.type_qualifiers:
            line += " " + " ".join(q.line() for q in self.type_qualifiers)
        if self.declarator:
            line += self.declarator.line()
        return line


class BitField(SimpleNode):
    """The size is a bitfield which indicates the number of bits in the struct
    this member takes up.

    http://en.cppreference.com/w/cpp/language/bit_field
    """
    __attrs__ = ("declarator", "size")
    __types__ = {
        "declarator": optional(Declarator),
        "size": optional(Expr),
    }
    __defaults__ = {
        "declarator": None,
        "size": None,
    }

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        assert self.declarator or self.size

    def line(self):
        line = ""
        if self.declarator:
            line += self.declarator.line()
            if self.size:
                line += " "
        if self.size:
            line += ": " + self.constexpr.line()
        return line


class RecordField(SimpleNode):
    __attrs__ = ("spec_qualifiers", "declarators")
    __types__ = {
        "spec_qualifiers": [SpecifierQualifier],
        "declarators": [(BitField, Declarator)],
    }

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        assert self.spec_qualifiers and self.declarators

    def line(self):
        return (" ".join(q.line() for q in self.spec_qualifiers) + " " +
                ", ".join(d.line() for d in self.declarators) + ";")


class RecordType(TypeSpecifier):
    __attrs__ = ("id", "struct_decls")
    __types__ = {
        "id": optional(str),
        "struct_decls": [RecordField],
    }
    __defaults__ = {"id": None}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        assert self.id or self.struct_decls

    def record_name(self):
        raise NotImplementedError

    def line(self):
        line = self.record_name()
        if self.id:
            line += " " + self.id
        if self.struct_decls:
            line += " {" + " ".join(d.line() for d in self.struct_decls) + "}"
        return line


class StructType(RecordType):
    def record_name(self):
        return "struct"


class UnionType(RecordType):
    def record_name(self):
        return "union"


class EnumField(SimpleNode):
    __attrs__ = ("id", "val")
    __types__ = {
        "id": str,
        "val": optional(Expr),
    }
    __defaults__ = {"val": None}

    def line(self):
        line = self.id
        if self.val:
            line += " = " + self.val.line()
        return line


class EnumType(TypeSpecifier):
    __attrs__ = ("id", "fields")
    __types__ = {
        "id": optional(str),
        "fields": [EnumField],
    }
    __defaults__ = {"id": None}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        assert self.id or self.fields

    def line(self):
        line = "enum"
        if self.id:
            line += " " + self.id
        if self.fields:
            line += " {" + ", ".join(e.line() for e in self.fields) + "}"
        return line


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


class Params(SimpleNode):
    __attrs__ = ("param_decls", "has_varargs")
    __types__ = {
        "param_decls": [ParamDecl],
        "has_varargs": bool,
    }
    __defaults__ = {
        "has_varargs": False,
    }

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        if self.has_varargs:
            assert self.param_decls

    def line(self):
        if self.has_varargs:
            return ", ".join(d.line() for d in self.param_decls) + ", ..."
        else:
            return ", ".join(d.line() for d in self.param_decls)


class IDs(SimpleNode):
    __attrs__ = ("ids",)
    __types__ = {"ids": [str]}
    __defaults__ = {"ids": []}

    def line(self):
        return ", ".join(self.ids)


class Scope(Declarator):
    __attrs__ = ("declarator",)
    __types__ = {"declarator": Declarator}

    def line(self):
        return "({})".format(self.declarator.line())


class ArrayDeclarator(Declarator):
    __attrs__ = ("direct_declarator", "size")
    __types__ = {
        "direct_declarator": optional(Declarator),
        "size": optional(Expr),
    }
    __defaults__ = {
        "direct_declarator": None,
        "size": None,
    }

    def line(self):
        line = ""
        if self.direct_declarator:
            line += self.direct_declarator.line()
        line += "["
        if self.size:
            line += self.size.line()
        line += "]"
        return line


class FuncDeclarator(Declarator):
    __attrs__ = ("direct_declarator", "arg_list")
    __types__ = {
        "direct_declarator": optional((str, Declarator)),
        "arg_list": optional((Params, IDs)),
    }
    __defaults__ = {
        "direct_declarator": None,
        "arg_list": None,
    }

    def line(self):
        line = ""
        if self.direct_declarator:
            line += str(self.direct_declarator)
        line += "("
        if self.arg_list:
            line += self.arg_list.line()
        line += ")"
        return line


class Stmt(Node):
    pass


class SimpleStmt(Stmt, SimpleNode):
    pass


class TypeName(SimpleNode):
    __attrs__ = ("spec_qualifiers", "abstract_declarator")
    __types__ = {
        "spec_qualifiers": [SpecifierQualifier],
        "abstract_declarator": optional(Declarator),
    }
    __defaults__ = {"abstract_declarator": None}

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        assert self.spec_qualifiers

    def line(self):
        line = " ".join(q.line() for q in self.spec_qualifiers)
        if self.abstract_declarator:
            line += " " + self.abstract_declarator.line()
        return line


class AtomicExpr(Expr):
    def precedence(self):
        return 0


class ID(AtomicExpr, Declarator):
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
        super().__init__(*args, **kwargs)
        assert len(self.c) == 1

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


class Not(Prec2Expr):
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
    __attrs__ = ("type", "expr")
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


class Addrof(Prec2Expr):
    __attrs__ = ("expr",)
    __types__ = {"expr": Expr}

    def line(self):
        return "&{}".format(self.scoped_line(self.expr))


class Sizeof(Prec2Expr):
    __attrs__ = ("val",)
    __types__ = {"val": (TypeName, Expr)}

    def line(self):
        return "sizeof({})".format(self.val.line())


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


class Xor(Prec9Expr, BinaryExpr):
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


class TernaryCond(Prec13Expr):
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


class Assign(AssignmentExpr):
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


class InitExpr(Expr):
    # Expression that can only be used in an InitAssign
    pass


class InitList(InitExpr):
    __attrs__ = ("elems",)
    __types__ = {"elems": [(InitExpr, Expr)]}
    __defaults__ = {"elems": []}

    def line(self):
        return "{" + ", ".join(e.line() for e in self.elems) + "}"


class InitAssign(SimpleStmt):
    __attrs__ = ("declarator", "initializer")
    __types__ = {
        "declarator": Declarator,
        "initializer": (InitList, Expr),
    }

    def line(self):
        return self.declarator.line() + " = " + self.initializer.line()


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
        return "{};".format(self.expr.line())


class If(Stmt):
    __attrs__ = ("cond", "body", "orelse")
    __types__ = {
        "cond": Expr,
        "body": [Stmt],
        "orelse": [Stmt],
    }
    __defaults__ = {
        "body": [],
        "orelse": [],
    }

    def lines(self):
        yield "if ({})".format(self.cond.line()) + " {"
        for stmt in self.body:
            for line in stmt.lines():
                yield INDENT_SIZE * " " + line

        if self.orelse:
            first_else = self.orelse[0]
            if not isinstance(first_else, If):
                yield "} else {"
                for stmt in self.orelse:
                    for line in stmt.lines():
                        yield INDENT_SIZE * " " + line
                yield "}"
            else:
                first_else_lines = first_else.lines()
                yield "} else " + next(first_else_lines)
                for line in first_else_lines:
                    yield line
                for stmt in self.orelse[1:]:
                    for line in stmt.lines():
                        yield line
        else:
            yield "}"


class While(Stmt):
    __attrs__ = ("cond", "body")
    __types__ = {
        "cond": Expr,
        "body": [Stmt],
    }
    __defaults__ = {"body": []}

    def lines(self):
        yield "while ({})".format(self.cond.line()) + " {"
        for stmt in self.body:
            for line in stmt.lines():
                yield INDENT_SIZE * " " + line
        yield "}"


class Dowhile(Stmt):
    __attrs__ = ("cond", "body")
    __types__ = {
        "cond": Expr,
        "body": [Stmt],
    }
    __defaults__ = {"body": []}

    def lines(self):
        yield "do {"
        for stmt in self.body:
            for line in stmt.lines():
                yield INDENT_SIZE * " " + line
        yield "}} while ({});".format(self.cond.line())



class Decl(SimpleStmt, ExternalDecl):
    __attrs__= ("decl_specifiers", "init_declarator_list")
    __types__ = {
        "decl_specifiers": [DeclSpecifier],
        "init_declarator_list": optional([(Declarator, InitAssign)]),
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


class For(Stmt):
    __attrs__ = ("init", "cond", "incr", "body")
    __types__ = {
        "init": optional((Decl, Expr)),
        "cond": optional(Expr),
        "incr": optional(Expr),
        "body": [Stmt],
    }
    __defaults__ = {
        "init": None,
        "cond": None,
        "incr": None,
        "body": [],
    }

    def lines(self):
        if isinstance(self.init, Decl):
            decl_part = self.init.line()
        elif isinstance(self.init, Expr):
            decl_part = self.init.line() + ";"
        else:
            decl_part = ";"

        if self.cond:
            cond_part = self.cond.line() + ";"
        else:
            cond_part = ";"

        if self.incr:
            incr_part = self.incr.line()
        else:
            incr_part = ""

        yield "for ({} {} {})".format(decl_part, cond_part, incr_part) + "{"
        for stmt in self.body:
            for line in stmt.lines():
                yield INDENT_SIZE * " " + line
        yield "}"


class Case(SimpleNode):
    __attrs__ = ("matches", "body")
    __types__ = {
        "matches": [Expr],
        "body": [Stmt],
    }

    def lines(self):
        for match in self.matches:
            yield "case {}:".format(match.line())
        for stmt in self.body:
            for line in stmt.lines():
                yield INDENT_SIZE * " " + line


class Default(SimpleNode):
    __attrs__ = ("body",)
    __types__ = {"body": [Stmt]}

    def lines(self):
        yield "default:"
        for stmt in self.body:
            for line in stmt.lines():
                yield INDENT_SIZE * " " + line


class Break(SimpleStmt):
    def line(self):
        return "break;"


class Switch(Stmt):
    __attrs__ = ("cond", "cases")
    __types__ = {
        "cond": Expr,
        "cases": [(Case, Default)],
    }

    def lines(self):
        yield "switch ({})".format(self.cond.line()) + "{"
        for case in self.cases:
            for line in case.lines():
                yield INDENT_SIZE * " " + line
        yield "}"


class FuncDef(ExternalDecl):
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


class Define(SimpleMacro):
    __attrs__ = ("id", "definition")
    __types__ = {
        "id": str,
        "definition": optional(str),
    }
    __defaults__ = {"definition": None}

    def line(self):
        if self.definition:
            return "#define {} {}".format(self.id, self.definition)
        else:
            return "#define {}".format(self.id)


class Ifdef(SimpleMacro):
    __attrs__ = ("id",)
    __types__ = {"id": str}

    def line(self):
        return "#ifdef {}".format(self.id)


class Ifndef(SimpleMacro):
    __attrs__ = ("id",)
    __types__ = {"id": str}

    def line(self):
        return "#ifndef {}".format(self.id)


class Endif(SimpleMacro):
    def line(self):
        return "#endif"


class TranslationUnit(Node):
    __attrs__ = ("external_decls",)
    __types__ = {"external_decls": [(ExternalDecl, Macro)]}
    __defaults__ = {"external_decls": []}

    def lines(self):
        for decl in self.external_decls:
            yield from decl.lines()
