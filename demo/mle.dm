// Set up data
num x[10];
x[0] = 5.839289;
x[1] = 3.195431;
x[2] = 12.526557;
x[3] = 5.596429;
x[4] = 5.453387;
x[5] = 6.330539;
x[6] = 4.712917;
x[7] = 7.884804;
x[8] = 7.263791;
x[9] = 5.166385;

// Set up loss function
symbol loss;
symbol theta;
loss = 1;
int i;
for (i = 0; i < 10; i = i + 1){
	loss = loss * (1 / (1 + (x[i] - theta) ^ 2));
}

// Define Newton's method
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

// Compute starting point
num avg = 0;
for (i = 0; i < 10; i = i + 1){
	avg = avg + x[i] / 10;
}

// Locate MLE
symbol partial[1];
partial[0] = theta;
num mle = univariateNewtonMethod(loss, partial, avg, 0.00001);
print("Found MLE:");
print(mle);