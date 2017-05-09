def streq(string a, string b) : bool {
	return strcompare(a, b) == 0;
}

def eval(symbol a) : num {
	num leftValue;
	num rightValue;
	num result;
	string op;
	if (isConstant(a) == 1){
		result = value(a);
	}
	// TODO check if initialized - if not, crash
	else {
		op = operator(a);
		if (streq(op, "PLUS")){
			leftValue = eval(left(a));
			rightValue = eval(right(a));
			result = leftValue + rightValue;
		}
		elseif (streq(op, "MINUS")){
			leftValue = eval(left(a));
			rightValue = eval(right(a));
			result = leftValue - rightValue;
		}
		elseif (streq(op, "TIMES")){
			leftValue = eval(left(a));
			rightValue = eval(right(a));
			result = leftValue * rightValue;
		}
		elseif (streq(op, "DIVIDE")){
			leftValue = eval(left(a));
			rightValue = eval(right(a));
			result = leftValue / rightValue;
		}
		elseif (streq(op, "EXP")){
			leftValue = eval(left(a));
			rightValue = eval(right(a));
			result = leftValue ^ rightValue;
		}
		elseif (streq(op, "LOG")){
			leftValue = eval(left(a));
			rightValue = eval(right(a));
			result = leftValue _ rightValue;
		}
		/*elseif (streq(op, "NEGATIVE")){
			// TODO currently, we won't reach here
			leftValue = eval(left(a));
			result = - leftValue;
		}*/
		/*else {
			// Crash the program
			print("Should crash here - invalid symbol operation (eval)");
		}*/
	}
	return result;
}

num e = 2.71828;

def partialDerivative(symbol out, symbol in) : num {
	num leftGrad;
	num rightGrad;
	string op;
	num dadL;
	num dadR;
	num L;
	num R;
	num result;

	if (isConstant(out) == 1){
		if (out == in){
			result = 1.0;
		}
		else{
			result = 0.0;
		}
	}
	else{
		op = operator(out);
		if (0 == 1 /*streq(op, "NEGATIVE")*/){
			leftGrad = partialDerivative(left(out), in);
			dadL = -1;
			result = dadL * leftGrad;
		}
		else {
			leftGrad = partialDerivative(left(out), in);
			rightGrad = partialDerivative(right(out), in);
				
			if (streq(op, "PLUS")){
				dadL = 1;
				dadR = 1;
			}
			elseif (streq(op, "MINUS")){
				dadL = 1;
				dadR = -1;
			}
			elseif (streq(op, "TIMES")){
				dadL = eval(right(out));
				dadR = eval(left(out));
			}
			elseif (streq(op, "DIVIDE")){
				dadL = 1 / eval(right(out));
				dadR = 0 - eval(left(out)) / (eval(right(out)) ^ 2);
			}
			elseif (streq(op, "EXP")){
				L = eval(left(out));
				R = eval(right(out));
				dadL = (R - 1) * L ^ R;
				dadR = e _ L * L ^ R;
			}
			elseif (streq(op, "LOG")){
				L = eval(left(out));
				R = eval(right(out));
				dadL = L _ e / R;
				dadR = L _ R * L _ e / L;
			}
			else {
				// Crash the program
				print("Should crash here - invalid symbol operation (partialDerivative)");
			}
			result = dadL * leftGrad + dadR * rightGrad;
		}
	}
	return result;
}

/*
def binaryChainRule(num dadL, num dadR, num leftGrad[], num rightGrad[], int n) : num[] {
	num grad[n];
	int i;
	for (i = 0; i < n; i = i + 1){
		grad[i] = dadL * leftGrad[i] + dadR * rightGrad[i];
	}
	return grad;
}

def unaryChainRule(num dadL, num leftGrad[], int n) : num[] {
	num grad[n];
	int i;
	for (i = 0; i < n; i = i + 1){
		grad[i] = dadL * leftGrad[i];
	}
	return grad;
}

def gradOfConstant(symbol a, symbol b[], int n) : num[] {
	num grad[n];
	int i;
	for (i = 0; i < n; i = i + 1){
		if (a == b[i]){
			grad[i] = 1;
		}
		else {
			grad[i] = 0;
		}
	}
	return grad;
}

def gradient(symbol a, symbol b[], int n) : num[] {
	num dadL;
	num dadR;
	num L;
	num R;
	num leftGrad[n];
	num rightGrad[n];
	string op;

	if (isConstant(a)){
		return gradOfConstant(a, b, n);
	}
	elseif (isNaked(a)){
		// Crash program

	}
	else {
		op = operator(a);
		if (streq(op, "NEGATIVE")){
			leftGrad = gradient(left(a), b, n);
			dadL = -1;
			return unaryChainRule(dadL, leftGrad, n);
		}
		else {
			leftGrad = gradient(left(a), b, n);
			rightGrad = gradient(right(a), b, n);
				
			if (streq(op, "PLUS"){
				dadL = 1;
				dadR = 1;
			}
			elseif (streq(op, "MINUS")){
				dadL = 1;
				dadR = -1;
			}
			elseif (streq(op, "TIMES")){
				dadL = eval(right(a));
				dadR = eval(left(a));
			}
			elseif (streq(op, "DIVIDE")){
				dadL = 1 / eval(right(a));
				dadR = - eval(left(a)) / (eval(right(a)) ^ 2);
			}
			elseif (streq(op, "EXP")){
				L = eval(left(a));
				R = eval(right(a));
				dadL = (R - 1) * L ^ R;
				dadR = exp() _ L * L ^ R;
			}
			elseif (streq(op, "LOG")){
				L = eval(left(a));
				R = eval(right(a));
				dadL = L _ exp() / R;
				dadR = L _ R * L _ exp() / L;
			}
			else {
				// Crash the program
				print("Should crash here - invalid symbol operation (eval)");
			}
			return binaryChainRule(dadL, dadR, leftGrad, rightGrad, n);
		}
	}
}
*/