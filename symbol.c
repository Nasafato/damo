#include <stdlib.h>
#include <stdio.h>

struct symbol {
	struct symbol *left;
	struct symbol *right;
	int isConstant;
	int isInitialized;
	double value;
	char *operator;
};

char *operator(struct symbol *a){
	return a->operator;
}

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
		printf("Malloc failed in createSymbol");
		exit(1);
	}
	a->left = 0;
	a->right = 0;
	a->isConstant = 0;
	a->isInitialized = 0;
	a->value = 0.0;
	return a;
}

struct symbol *createRoot(struct symbol *l, struct symbol *r, char *op){
	struct symbol *a = createSymbol();
	a->left = l;
	a->right = r;
	a->operator = op;
	a->isInitialized = 1;
	return a;
}

struct symbol *setSymbolValue(struct symbol *a, double val){
	a->value = val;
	a->isConstant = 1;
	a->isInitialized = 1;
	a->left = 0;
	a->right = 0;
	return a;
}