code = """char x;

list[int] func(){
    int x;
    list[int] l;
    l.append(x);
    return l;
}

int main(){
    list[int] l = func();
    list[int] l2;
    l2 = l;
    print(l2);
}
    """

from compiler.compiler import Compiler


def main():
    compiler = Compiler()
    compiler.compile(code)
    print(compiler.lang_ast())


if __name__ == "__main__":
    main()
