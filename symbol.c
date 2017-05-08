#include <stdlib.h>
#include <stdio.h>

struct symbol {
	symbol *left;
	symbol *right;
	int isConstant;
	int isInitialized;
	double value;
};

struct symbol *left(struct symbol *a){
	return a->leftChild;
}

struct symbol *right(struct symbol *a){
	return a->rightChild;
}

int isConstant(struct symbol *a){
	return a->isConstant;
}

int isInitialized(struct *a){
	return a->isInitialized;
}

double value(struct symbol *a){
	return a->value;
}

struct symbol *createSymbol(){
	symbol *a = malloc(sizeof(struct symbol));
	if (a == 0){
		printf("Malloc failed");
		exit(1);
	}
	a->left = 0;
	a->right = 0;
	a->isConstant = 0;
	a->isInitialized = 0;
	a->value = 0.0;
	return a;
}