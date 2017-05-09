string myString1 = "Hello";
string myString2 = "world";
string myString3;
myString3 = "world";

if (strcompare(myString1, myString2) != 0){
	print("Right");
}
else{
	print("Wrong");
}

if (strcompare(myString2, myString3) == 0){
	print("Right");
}
else{
	print("Wrong");
}