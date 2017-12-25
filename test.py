from cast.nodes import *


def main():
    m = TranslationUnit([
        FunctionDef(
            decl_specifiers=[
                SimpleTypeSpecifier("int"),
            ],
            declarator=Declarator(
                direct_declarator=FunctionDeclarator(
                    direct_declarator=IDDeclarator("main")
                )
            )
        )
    ])
    print(m)


if __name__ == "__main__":
    main()
