symbol a;

def test(): symbol {
    symbol x;
    symbol z;
    x = 15;
    z = x + 2.0;

    return z;
}

a = test( );

print_num( value(right(a)));
