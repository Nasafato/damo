symbol a;

def test(symbol tmp): symbol {
    symbol x;
    symbol z;
    x = 15;
    z = x + 2.0;
    tmp = z + 2;

    return tmp;
}

a = test(a);

print_num( value(right(a)));
