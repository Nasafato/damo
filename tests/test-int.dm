int a=3;
def test() : int {
	// Variable declarations come first
	int i;
	int j;

	// Statements come second
	j = 2;
	i = 1;
	print_int(i);
	print_int(j);
        print_int(a+j);
	return 0;
}

test();
