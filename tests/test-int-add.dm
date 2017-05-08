def test() : int {
	// Variable declarations come first
	int i;
	int j;
	int k;

	// Statements come second
	j = 2;
	i = 1;
	k = j + i;
	print_int(k);
	print_int(3);
	return 0;
}

test();
