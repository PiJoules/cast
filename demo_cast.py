from cast.nodes import *


def main():
    m = TranslationUnit([
        # Macros
        Include("stdio.h", True),
        Include("path/to/header.h"),
        Ifndef("DAYS_IN_YEAR"),
        Define("DAYS_IN_YEAR", "365"),
        Endif(),
        Ifdef("UNKNOWN_VAR"),
        Endif(),

        # Enum declaration
        Decl(
            [EnumType("days", [
                EnumField("SUN", Int(1)),
                EnumField("MON"),
                EnumField("TUE"),
                EnumField("WED"),
                EnumField("THU"),
                EnumField("FRI"),
                EnumField("SAT"),
            ])]
        ),

        # Function declaration
        Decl(
            [Type("void")],
            [FuncDeclarator("function_1")]
        ),
        Decl(
            [Type("void")],
            [FuncDeclarator("function_2", IDs(["void"]))]
        ),
        Decl(
            [Type("int")],
            [FuncDeclarator("add_two_ints", Params([
                ParamDecl([Type("int")], ID("x1")),
                ParamDecl([Type("int")], ID("x2")),
            ]))]
        ),

        # Function definition
        FuncDef(
            [Type("int")],
            FuncDeclarator("main", Params([
                ParamDecl([Type("int")], ID("argc")),
                ParamDecl([Type("char")], Pointer(Pointer(ID("argv")))),
            ])),
            [
                # Variable declaration; no assigmnent
                Decl(
                    [Type("int")],
                    [ID("var")]
                ),
                # Variable declaration with assigmnent
                Decl(
                    [Type("int")],
                    [InitAssign(ID("x_int"), Int(0))]
                ),
                Decl(
                    [Type("short")],
                    [InitAssign(ID("x_short"), Int(0))]
                ),
                Decl(
                    [Type("char")],
                    [InitAssign(ID("x_char"), Int(0))]
                ),
                Decl(
                    [Type("char")],
                    [InitAssign(ID("y_char"), Char("y"))]
                ),
                Decl(
                    [Type("long")],
                    [InitAssign(ID("x_long"), Int(0))]
                ),
                # All type specifiers in a declaration are concatenated
                Decl(
                    [Type("long"), Type("long")],
                    [InitAssign(ID("x_long_long"), Int(0))]
                ),
                Decl(
                    [Type("float")],
                    [InitAssign(ID("x_float"), Float(0.0))]
                ),
                Decl(
                    [Type("double")],
                    [InitAssign(ID("x_double"), Float(0.0))]
                ),
                Decl(
                    [Type("unsigned"), Type("long long")],
                    [ID("ux_long_long")]
                ),

                ExprStmt(Char('0')),
                ExprStmt(Char('A')),

                Decl(
                    [Type("size_t")],
                    [InitAssign(ID("size"), Sizeof(PostInc(ID("a"))))]
                ),

                # Call
                ExprStmt(Call(ID("printf"), [Str("%d\\n"), ID("y")])),
                ExprStmt(Call(ID("printf"), [Str("%zu\\n"), Sizeof(ID("int"))])),

                # Arrays
                Decl(
                    [Type("char")],
                    [ArrayDeclarator(ID("my_char_array"), Int(20))]
                ),
                Decl(
                    [Type("int")],
                    [ArrayDeclarator(ID("my_char_array"), ID("size"))]
                ),
                Decl(
                    [Type("char")],
                    [InitAssign(
                        ArrayDeclarator(ID("my_array"), Int(20)),
                        InitList([Int(0)])
                    )]
                ),

                # Assignment + Subscript/Array access
                ExprStmt(Subscript(ID("my_array"), Int(0))),
                ExprStmt(Assign(
                    Subscript(ID("my_array"), Int(1)),
                    Int(2)
                )),
                Decl(
                    [Type("char")],
                    [InitAssign(
                        ArrayDeclarator(ID("a_string")),
                        Str("This is a string.")
                    )]
                ),

                # Multi-dimensional arrays
                Decl(
                    [Type("int")],
                    [InitAssign(
                        ArrayDeclarator(ArrayDeclarator(ID("multi_array"), Int(2)), Int(3)),
                        InitList([
                            InitList([Int(1), Int(2), Int(3)]),
                            InitList([Int(4), Int(5), Int(6)]),
                        ])
                    )]
                ),
                Decl(
                    [Type("int")],
                    [InitAssign(
                        ID("array_int"),
                        Subscript(Subscript(ID("multi_array"), Int(0)), Int(2))
                    )]
                ),

                # Multiple declaration of same type
                Decl(
                    [Type("int")],
                    [
                        InitAssign(ID("i1"), Int(1)),
                        InitAssign(ID("i2"), Int(2)),
                    ]
                ),
                Decl(
                    [Type("float")],
                    [
                        InitAssign(ID("f1"), Int(1)),
                        ID("f2"),
                    ]
                ),
                Decl(
                    [Type("int")],
                    [ID("b"), ID("c")]
                ),

                # Arithmetic
                ExprStmt(Add(ID("i1"), ID("i2"))),
                ExprStmt(Sub(ID("i1"), ID("i2"))),
                ExprStmt(Mul(ID("i1"), ID("i2"))),
                ExprStmt(Div(ID("i1"), ID("i2"))),

                # Casting
                ExprStmt(Div(Cast(
                    TypeName([Type("float")]),
                    ID("i1")
                ), ID("i2"))),
                ExprStmt(Div(
                    ID("i1"),
                    Cast(TypeName([Type("float")]), ID("i2"))
                )),

                # Other binary expressions
                ExprStmt(Mod(Int(11), Int(3))),
                ExprStmt(Eq(Int(11), Int(3))),
                ExprStmt(Ne(Int(11), Int(3))),
                ExprStmt(Gt(Int(11), Int(3))),
                ExprStmt(Lt(Int(11), Int(3))),
                ExprStmt(Lte(Int(11), Int(3))),
                ExprStmt(Gte(Int(11), Int(3))),

                # Parenthesis for scoping are added when appropriate
                ExprStmt(Mul(Add(Int(1), Int(2)), Int(3))),  # (1 + 2) * 3
                ExprStmt(Add(Mul(Int(1), Int(2)), Int(3))),  # 1 * 2 + 3

                # Logical expressions
                ExprStmt(Not(Int(3))),
                ExprStmt(And(Int(11), Int(3))),
                ExprStmt(Or(Int(11), Int(3))),

                # Ternary conditional operator (?:)
                ExprStmt(TernaryCond(ID("a"), ID("b"), ID("c"))),

                # Post/pre inc/dec operators
                ExprStmt(PreInc(ID("x"))),
                ExprStmt(PreDec(ID("x"))),
                ExprStmt(PostInc(ID("x"))),
                ExprStmt(PostDec(ID("x"))),

                # Bitwise expressions
                ExprStmt(BitNot(Int(3))),
                ExprStmt(BitAnd(Int(11), Int(3))),
                ExprStmt(BitOr(Int(11), Int(3))),
                ExprStmt(Xor(Int(11), Int(3))),
                ExprStmt(LShift(Int(11), Int(3))),
                ExprStmt(RShift(Int(11), Int(3))),

                # Control flow
                If(
                    ID("cond"),
                    [ExprStmt(Call(ID("expr")))]
                ),
                If(
                    ID("cond"),
                    [ExprStmt(Call(ID("expr")))],
                    [ExprStmt(Call(ID("else_expr")))],
                ),

                # If/Else ladders are implemented as nested if stmts
                If(
                    ID("cond"),
                    [ExprStmt(Call(ID("expr")))],
                    [If(
                        ID("cond2"),
                        [ExprStmt(Call(ID("cond2_expr")))],
                    )],
                ),
                # If/Else if/Else
                If(
                    ID("cond"),
                    [ExprStmt(Call(ID("expr")))],
                    [If(
                        ID("cond2"),
                        [ExprStmt(Call(ID("cond2_expr")))],
                        [ExprStmt(Call(ID("final_else_expr")))],
                    )],
                ),
                If(
                    ID("cond"),
                    [If(
                        ID("cond2"),
                        [ExprStmt(Call(ID("first_nested_expr")))],
                        [ExprStmt(Call(ID("final_nested_expr")))]
                    )]
                ),

                # While
                While(
                    ID("cond"),
                    [ExprStmt(Call(ID("func")))]
                ),

                # Do-while
                Dowhile(
                    ID("cond"),
                    [ExprStmt(Call(ID("func")))]
                ),

                # For loop
                For(
                    Decl([Type("int")], [InitAssign(ID("i"), Int(0))]),
                    Lt(ID("i"), Int(10)),
                    PreInc(ID("i")),
                    [ExprStmt(Call(ID("func")))]
                ),

                # For without declaration in init
                Decl([Type("int")], [ID("i")]),
                For(
                    Assign(ID("i"), Int(0)),
                    Lt(ID("i"), Int(10)),
                    PreInc(ID("i")),
                    [ExprStmt(Call(ID("func")))]
                ),

                # Infinite for loop
                For(),

                # Switch
                Switch(ID("cond"), [
                    Case([Int(0)], [ExprStmt(Call(ID("func"))), Break()]),
                    Case([Int(1)], [ExprStmt(Call(ID("func2"))), Break()]),
                    Case([Int(2), Int(3)], [ExprStmt(Call(ID("func3"))), Break()]),
                    Default([ExprStmt(Call(ID("default_dunc")))])
                ]),

                # Typedef
                Decl(
                    [Typedef(), EnumType(fields=[
                        EnumField("false"),
                        EnumField("true"),
                    ])],
                    [ID("bool")]
                ),

                # Struct decl
                Decl(
                    [StructType("s", [
                        RecordField(
                            [Type("char")],
                            [ID("c")]
                        ),
                        RecordField(
                            [Type("int")],
                            [Pointer(ID("p"))]
                        ),
                    ])],
                ),

                # Struct decl
                Decl(
                    [UnionType("u", [
                        RecordField(
                            [Type("char")],
                            [ID("c")]
                        ),
                        RecordField(
                            [Type("int")],
                            [Pointer(ID("p"))]
                        ),
                    ])],
                ),

                # Abstract types
                # Pointer to function that takes a char and int and returns an
                # int
                ExprStmt(Cast(
                    TypeName(
                        [Type("int")],
                        FuncDeclarator(Scope(Pointer()), IDs(["char", "int"]))
                    ),
                    ID("p")
                )),

                # Array of size 3 that holds int pointers
                ExprStmt(Cast(
                    TypeName(
                        [Type("int")],
                        ArrayDeclarator(Pointer(), Int(3))
                    ),
                    ID("p")
                )),

                # Pointer to an array of size 3 that holds ints
                ExprStmt(Cast(
                    TypeName(
                        [Type("int")],
                        ArrayDeclarator(Pointer(), Int(3))
                    ),
                    ID("p")
                )),
            ]
        )
    ])
    print(m)


if __name__ == "__main__":
    main()
