def test(int x): int {
    if(x==1){
	return 1;
    } 

    else{ 
	return x * test(x-1);
    }
}

test(5);