// Example 1
symbol a;
symbol b;
symbol c;

a = b + c;
b = 1.0;
c = 2;

print_num(eval(b));
print_num(eval(c));
print_num(eval(a));

// Example 2
symbol d;
symbol e;
symbol f;

d = e * f;
e = 3;
f = 4;

print_num(eval(e));
print_num(eval(f));
print_num(eval(d));

// Example 3
symbol g;

g = a ^ d;

print_num(eval(g));