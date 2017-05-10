symbol a;
symbol b;
symbol c;
symbol d;

b = 2 + d;
c = 3 + d;
d = 1;
a = b + c;

if (eval(a) == 7.){
	print("Right");
}
else{
	print("Wrong");
}
