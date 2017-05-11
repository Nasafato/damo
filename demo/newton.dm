def univariateNewtonMethod(symbol out, symbol in, num start, num threshold) : num {
	num x1 = start;
	print_num(x1);
	in = x1;
	num result = eval(out);
	num deriv = partialDerivative(out, in);
	print_num(result);
	print_num(deriv);
	num x2 = x1 - result / deriv;
	
	while(absNum(x1 - x2) > threshold){
		x1 = x2;
		print_num(x1);
		in = x1;
		result = eval(out);
		deriv = partialDerivative(out, in);
		x2 = x1 - result / deriv;
	}
	return x1;
}

symbol a;
symbol b;
num root;

// Locate square root of 10
a = b ^ 2 - 10;
root = univariateNewtonMethod(a, b, 1.0, 0.000001);
print("Square root of 10:");
print_num(root);

// Locate root of cubic polynomial
a = b ^ 3 - b + 1;
root = univariateNewtonMethod(a, b, 0.0 - 2.0, 0.000001);
print("Root of polynomial:");
print_num(root);