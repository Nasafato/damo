symbol a;
symbol b;
symbol c;

a = 5;
b = 2;
c = a ^ b;

if (eval(c) == 25.){
	print("Right");
}
else{
	print("Wrong");
}

c = 4 ^ b;

if (eval(c) == 16.){
	print("Right");
}
else{
	print("Wrong");
}

c = b ^ 3;

if (eval(c) == 8.){
	print("Right");
}
else{
	print("Wrong");
}