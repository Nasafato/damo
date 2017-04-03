def test_mod_bop() : int {
	// Variable declarations come first
	
	int i;
	int j;
	num k;
	num h;
	k = 4.0;
	h = 2.0;
	i = 4;
	j = 2;

	print_int( i % j );
	print_num( k % h );
	
	return 0;
}


test_mod_bop();