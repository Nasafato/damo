symbol a;
symbol b;
num result;
num deriv;

b = -2;
a = b ^ 3;

result = eval(a);
deriv = partialDerivative(a, b);

if (result == 0.0-8.0  && deriv == 12.0){
	print("Right");
}
else{
	print("Wrong");
}

b = 3;

result = eval(a);
deriv = partialDerivative(a, b);

if (result == 27.0 && deriv == 27.0){
	print("Right");
}
else{
	print("Wrong");
}

b = 5;
a = exponentialConstant ^ b;

result = eval(a);
deriv = partialDerivative(a, b);

if (result == exponentialConstant ^ 5  && deriv == exponentialConstant ^ 5){
	print("Right");
}
else{
	print("Wrong");
}

b = 3;
a = b * 2;

result = eval(a);
deriv = partialDerivative(a, b);

if (result == 6.0 && deriv == 2.0){
	print("Right");
}
else{
	print("Wrong");
}

a = b + 1;

result = eval(a);
deriv = partialDerivative(a, b);

if (result == 4.0 && deriv == 1.0){
	print("Right");
}
else{
	print("Wrong");
}

a = b * b;

result = eval(a);
deriv = partialDerivative(a, b);

if (result == 9.0 && deriv == 6.0){
	print("Right");
}
else{
	print("Wrong");
}

b = 4;
a = 2 _ b;

result = eval(a);
deriv = partialDerivative(a, b);

if (result == 2.0 && deriv == 2 _ exponentialConstant / 4){
	print("Right");
}
else{
	print("Wrong");
}

b = 2;
a = b _ 4;

result = eval(a);
deriv = partialDerivative(a, b);

if (result == 2.0 && deriv == 2 _ exponentialConstant){
	print("Right");
}
else{
	print("Wrong");
}