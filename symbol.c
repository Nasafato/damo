#include <stdlib.h>
#include <stdio.h>

struct symbol {
	struct symbol *left;
	struct symbol *right;
	int isConstant;
	int isInitialized;
	double value;
};

struct symbol *left(struct symbol *a){
	return a->left;
}

struct symbol *right(struct symbol *a){
	return a->right;
}

int isConstant(struct symbol *a){
	return a->isConstant;
}

int isInitialized(struct symbol *a){
	return a->isInitialized;
}

double value(struct symbol *a){
	return a->value;
}

struct symbol *createSymbol(){
	struct symbol *a = malloc(sizeof(struct symbol));
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

struct symbol *createConstSymbol(double val){
	struct symbol *a = createSymbol();
	a->value = val;
	a->isConstant = 1;
	return a;
}