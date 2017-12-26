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
                ParamDecl([Type("char")], ID("argv")),
                #ParamDecl([Type("char")], Pointer(Pointer(ID("argv")))),
            ])),
            [
                # Variable declaration; no assigmnent
                Decl(
                    [Type("int")],
                    [ID("x")]
                ),
                # Variable declaration with assigmnent
                Decl(
                    [Type("int")],
                    [InitAssign(ID("y"), Int(10))]
                ),

                # Call
                ExprStmt(Call(ID("printf"), [Str("%d\\n"), ID("y")])),

                # Struct decl
                Decl(
                    [StructType("s", [
                        StructField(
                            [Type("char")],
                            [StructDeclarator(ID("c"))]
                        ),
                    ])],
                ),
            ]
        )
    ])
    print(m)


if __name__ == "__main__":
    main()
