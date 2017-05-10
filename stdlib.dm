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
	elseif (isInitialized(a) != 1){
		print("Evaluating an uninitialized symbol");
	}
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
		else {
			// Crash the program
			print("Unknown operator");
		}
	}
	return result;
}

num exponentialConstant = 2.71828;

def partialDerivative(symbol out, symbol in) : num {
	num leftGrad;
	num rightGrad;
	string op;
	num dadL = 0;
	num dadR = 0;
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
	elseif(isInitialized(out) != 1){
		print("Attempting to differentiate uninitialized symbol");
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
				dadR = 0.0 - 1.0;
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
				if (leftGrad != 0.0){
					dadL = R * (L ^ (R - 1));
				}
				if (rightGrad != 0.0){
					dadR = (exponentialConstant _ L) * (L ^ R);
				}
			}
			elseif (streq(op, "LOG")){
				L = eval(left(out));
				R = eval(right(out));
				if (leftGrad != 0.0){
					dadL = (L _ R) * (L _ exponentialConstant) / L;
				}
				if (rightGrad != 0.0){
					dadR = (L _ exponentialConstant) / R;
				}	
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