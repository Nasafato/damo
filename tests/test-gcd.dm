def gcd(int a, int b): int {
    while (a != b) {
        if (a > b)  {
            a = a - b;
        } else {
            b = b - a;
        }
    }
    return a;
}

print_int(gcd(2, 14));
print_int(gcd(3, 15));
print_int(gcd(99, 121));