def foo(): void {

}

def bar(int a, bool b, int c): int {
    return a + c;
}

print_int(bar(17, false, 25));