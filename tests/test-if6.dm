def cond(bool b): int {
    int x;
    if (b) {
        x = 42;
    } else {
        x = 17;
    }
    return x;
}

print_int(cond(true));
print_int(cond(false));