code = """# char y;

List[int] func(){
    int x = 2;
    List[int] l;
    l.append(x);
    return l;
}

int start(){
    List[int] l = func();
    List[int] l2;
    l2 = l;
    print(l2);
}
    """

from compiler.compiler import Compiler


def main():
    compiler = Compiler()
    compiler.compile(code)

    print("-------- Original ---------")
    print(compiler.lang_ast())

    print("\n-------- __main__ ---------")
    print(compiler.c_ast())

    with open("test.cc", "w") as f:
        f.write(str(compiler.c_ast()))

    for fname in compiler.generated_files():
        print("\n-------- {} ---------".format(fname))
        with open(fname, "r") as f:
            print(f.read())


if __name__ == "__main__":
    main()
