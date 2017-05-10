// Example 1
symbol a;

symbol partials1[1];
partials1[0] = a;
num grad[];
grad = gradient(a, partials, 1);

print(grad[0]);

// Example 2
symbol b;
symbol c;

a = b * c;
b = 4;
c = 5;

symbol partials[2];
partials2[0] = b; partials2[1] = c;
grad = gradient(a, partials2, 2);

print(grad[0]); print(grad[1]);

// Example 3
symbol d; symbol e; symbol f; symbol g;

g = a / d;
d = e _ f;
e = 5;
f = 25;

symbol partials3[4];
partials3[0] = b; partials3[1] = c; partials3[2] = e; partials3[3] = f;
grad = gradient(g, partials3, 4);

print(grad[0]); print(grad[1]); print(grad[2]); print(grad[3]);