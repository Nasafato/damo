def cond(bool b) {
    int x;
    if (b) {
        x = 42;
    } else {
        x = 17;
    }
    return x;
}

print(cond(true))
print(cond(false))