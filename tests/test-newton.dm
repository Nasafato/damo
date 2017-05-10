def univariateNewtonMethod(symbol out, symbol in, num start, num threshold) : num {
	// Initial iteration
	num x1 = start;
	print_num(x1);
	in = x1;
	num x2 = x1 - eval(out) / partialDerivative(out, in);
	// Repeat iteratively
	while( (((x1 - x2)^2)^0.5) > threshold){
		x1 = x2;
		print_num(x1);
		in = x1;
		x2 = x1 - eval(out) / partialDerivative(out, in);
	}
	return x1;
}

// Approximate square root of 10
symbol a; 
symbol b;
a = b ^ 2 - 10;
num root = univariateNewtonMethod(a, b, 1.0, 0.00001);
print("Square root of 10");
print_num(root);

// Locate root of cubic polynomial
a = b ^ 3 - b + 1;
root = univariateNewtonMethod(a, b, 0 - 1.0, 0.00001);
print("Root of polynomial");
print_num(root);
