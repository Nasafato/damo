symbol a;
symbol b;
symbol c;
symbol d;
symbol e;
symbol f;
symbol g;

b = c * d;
e = f / g;

c = 2;
d = 3;

f = 8;
g = 4;

a = e ^ b;

if (eval(a) == 64.0){
	print("Right");
}
else{
	print("Wrong");
}

f = 7.5;
g = 2.5;

if (eval(a) == 729.0){
	print("Right");
}
else{
	print("Wrong");
}