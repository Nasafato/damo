def test() : int {
	// Variable declarations come first
	int i;
	int j;
	num k;
	num l;	
	
	k = 1.0;
	l = 2.0;
	j = 2;
	i = 4;

	print_int( i/j );
	print_int( i + j );
	print_num(k*l);
	print_num(l-k);
	

	return 0;
}

test();