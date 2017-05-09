def plusOne( int ss, int k ) : int {
	//int vv = 1;
	if (k == 0){
		return ss;
	}
	else{
		return plusOne(ss, k - 1);
	}

}

//int i;
//i = 3;
//i = plusOne( i, 1 );
plusOne(1, 2);
//print_int( i );

def fact(num j) : num {
	if (j < 0){
		return j;
	}
	return fact(j - 1);
}