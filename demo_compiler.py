from compiler.compiler import lexer, parser


def main():
    code = """
char x;

list[int] func(){
    int x;
    list[int] l;
}

int main(){
}
    """
    m = parser.parse(code)
    print(m)


if __name__ == "__main__":
    main()
