symbol a;
symbol b;
a = b + 1;
b = 1;
if (eval(a) == 2.0){
	print("Right");
}
else{
	print("Wrong");
}

b = 2;
if (eval(a) == 3.0){
	print("Right");
}
else{
	print("Wrong");
}