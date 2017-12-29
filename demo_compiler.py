code = """char x;

list[uint32] func(){
    uint32 x = 2;
    list[uint32] l;
    l.append(x);
    return l;
}

uint32 main(){
    list[uint32] l = func();
    list[uint32] l2;
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

    for fname in compiler.generated_files():
        print("\n-------- {} ---------".format(fname))
        with open(fname, "r") as f:
            print(f.read())


if __name__ == "__main__":
    main()
