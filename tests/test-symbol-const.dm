







def streq(string a, string b) : bool {
	return strcompare(a, b) == 0;
}

symbol a;
symbol b;
symbol c;
symbol d;
d = 1;

a = 1 + b;


def sym_test( symbol node ) : int {
	symbol e;
	node = 1 + e;
	return 0;
}

sym_test( c );

symbol f;
symbol g;
symbol h;
g = 1.0;
h = 2.0;
f = g+h;
//eval(f);

bool temp;
temp = streq("A","A");

if(  temp ){
	print("Hi");
}

print_num(value(right(f)));
print(operator(f));
print_int(isConstant(g));
