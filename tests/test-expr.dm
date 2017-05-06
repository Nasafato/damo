



//int j = 2;
def test() : int {
	// Variable declarations come first
	int i;
	int k;
	int j;

	j = 1;
	// Statements come second
	j = 2;
	i = 1;
	k = j + i + 3;
	print_int(k);
	print_int(3);
	return 0;
}

def lol() : int {
	// Variable declarations come first
	int i;
	int k;
	//int j;

	// Statements come second
	//j = 2;
	i = 1;
	k = j + i + 3;
	print_int(k);
	print_int(3);
	return 0;
}




test();

lol();