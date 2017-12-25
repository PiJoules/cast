from cast.nodes import *


def main():
    m = TranslationUnit([
        # Includes
        Include("stdio.h", True),

        # Function definition
        FunctionDef(
            decl_specifiers=[
                SimpleTypeSpecifier("int"),
            ],
            declarator=Declarator(
                direct_declarator=FunctionDeclarator(
                    direct_declarator=IDDeclarator("main")
                )
            ),
            stmts=[
                # Variable declaration; no assigmnent
                Declaration(
                    [SimpleTypeSpecifier("int")],
                    [InitDeclarator(Declarator(IDDeclarator("x")))]
                ),
                # Variable declaration with assigmnent
                Declaration(
                    [SimpleTypeSpecifier("int")],
                    [InitDeclarator(Declarator(IDDeclarator("y")), Int(10))]
                ),

                # Call
                ExprStmt(Call(ID("printf"), [Str("%d\\n"), ID("y")])),
            ]
        )
    ])
    print(m)


if __name__ == "__main__":
    main()
