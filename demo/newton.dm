def univariateNewtonMethod(symbol out, symbol in[], num start, num threshold) : num {
	// Initial iteration
	num x1 = start;
	print(x1);
	in[0] = x1;
	num x2 = x1 - eval(out) / grad(out, in, 1)[0];
	// Repeat iteratively
	while(abs(x1 - x2) > threshold){
		x1 = x2;
		print(x1);
		in[0] = x1;
		x2 = x1 - eval(out) / grad(out, in, 1)[0];
	}
	return x1;
}

// Approximate square root of 10
symbol a; symbol b;
a = b ^ 2 - 10;
num partials1[1];
partials[0] = b;
num root = univariateNewtonMethod(a, partials1, 1, 0.00001);
print("Square root of 10");
print(root);

// Locate root of cubic polynomial
a = b ^ 3 - b + 1;
num root = univariateNewtonMethod(a, partials1, -1, 0.00001);
print("Root of polynomial");
print(root);