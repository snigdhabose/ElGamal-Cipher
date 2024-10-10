#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define	MAXDIGITS	10000		/* maximum length bignum */

#define PLUS		1		/* positive sign bit */
#define MINUS		-1		/* negative sign bit */

typedef struct {
        char digits[MAXDIGITS];         /* represent the number */
	int signbit;			/* 1 if positive, -1 if negative */
        int lastdigit;			/* index of high-order digit */
} bignum;


void print_bignum(bignum *n)
{
	int i;

	if (n->signbit == MINUS) printf("- ");
	for (i=n->lastdigit; i>=0; i--)
		printf("%c",'0'+ n->digits[i]);
}

void int_to_bignum(int s, bignum *n)
{
	int i;				/* counter */
	int t;				/* int to work with */

	if (s >= 0) n->signbit = PLUS;
	else n->signbit = MINUS;

	for (i=0; i<MAXDIGITS; i++) n->digits[i] = (char) 0;

	n->lastdigit = -1;

	t = abs(s);

	while (t > 0) {
		n->lastdigit ++;
		n->digits[ n->lastdigit ] = (t % 10);
		t = t / 10;
	}

	if (s == 0) n->lastdigit = 0;
}

void initialize_bignum(bignum *n)
{
	int_to_bignum(0,n);
}


int max(int a, int b)
{
	if (a > b) return(a); else return(b);
}


void add_bignum(bignum *a, bignum *b, bignum *c)
{
	int carry;			/* carry digit */
	int i;				/* counter */

	initialize_bignum(c);

	if (a->signbit == b->signbit) c->signbit = a->signbit;
	else {
		if (a->signbit == MINUS) {
			a->signbit = PLUS;
			subtract_bignum(b,a,c);
			a->signbit = MINUS;
		} else {
                        b->signbit = PLUS;
                        subtract_bignum(a,b,c);
                        b->signbit = MINUS;
		}
		return;
	}

	c->lastdigit = max(a->lastdigit,b->lastdigit)+1;
	carry = 0;

	for (i=0; i<=(c->lastdigit); i++) {
		c->digits[i] = (char) (carry+a->digits[i]+b->digits[i]) % 10;
		carry = (carry + a->digits[i] + b->digits[i]) / 10;
	}

	zero_justify(c);
}


void subtract_bignum(bignum *a, bignum *b, bignum *c)
{
	int borrow;
	int v;
	int i;

	initialize_bignum(c);

	if ((a->signbit == MINUS) || (b->signbit == MINUS)) {
                b->signbit = -1 * b->signbit;
                add_bignum(a,b,c);
                b->signbit = -1 * b->signbit;
		return;
        }

	if (compare_bignum(a,b) == PLUS) {
		subtract_bignum(b,a,c);
		c->signbit = MINUS;
		return;
	}

        c->lastdigit = max(a->lastdigit,b->lastdigit);
        borrow = 0;

        for (i=0; i<=(c->lastdigit); i++) {
		v = (a->digits[i] - borrow - b->digits[i]);
		if (a->digits[i] > 0)
			borrow = 0;
		if (v < 0) {
			v = v + 10;
			borrow = 1;
		}

                c->digits[i] = (char) v % 10;
        }

	zero_justify(c);
}

int compare_bignum(bignum *a, bignum *b)
{
	int i;				/* counter */

	if ((a->signbit == MINUS) && (b->signbit == PLUS)) return(PLUS);
	if ((a->signbit == PLUS) && (b->signbit == MINUS)) return(MINUS);

	if (b->lastdigit > a->lastdigit) return (PLUS * a->signbit);
	if (a->lastdigit > b->lastdigit) return (MINUS * a->signbit);

	for (i = a->lastdigit; i>=0; i--) {
		if (a->digits[i] > b->digits[i]) return(MINUS * a->signbit);
		if (b->digits[i] > a->digits[i]) return(PLUS * a->signbit);
	}

	return(0);
}

void zero_justify(bignum *n)
{
	while ((n->lastdigit > 0) && (n->digits[ n->lastdigit ] == 0))
		n->lastdigit --;

        if ((n->lastdigit == 0) && (n->digits[0] == 0))
		n->signbit = PLUS;
}


void digit_shift(bignum *n, int d)
{
	int i;

	if ((n->lastdigit == 0) && (n->digits[0] == 0)) return;

	for (i=n->lastdigit; i>=0; i--)
		n->digits[i+d] = n->digits[i];

	for (i=0; i<d; i++) n->digits[i] = 0;

	n->lastdigit = n->lastdigit + d;
}



void multiply_bignum(bignum *a, bignum *b, bignum *c)
{
	bignum row;			/* represent shifted row */
	bignum tmp;			/* placeholder bignum */
	int i,j;			/* counters */

	initialize_bignum(c);

	row = *a;

	for (i=0; i<=b->lastdigit; i++) {
		for (j=1; j<=b->digits[i]; j++) {
			add_bignum(c,&row,&tmp);
			*c = tmp;
		}
		digit_shift(&row,1);
	}

	c->signbit = a->signbit * b->signbit;

	zero_justify(c);
}


void divide_bignum(bignum *a, bignum *b, bignum *c)
{
        bignum row;                     /* represent shifted row */
        bignum tmp;                     /* placeholder bignum */
	int asign, bsign;		/* temporary signs */
        int i,j;                        /* counters */

	initialize_bignum(c);

	c->signbit = a->signbit * b->signbit;

	asign = a->signbit;
	bsign = b->signbit;

	a->signbit = PLUS;
        b->signbit = PLUS;

	initialize_bignum(&row);
	initialize_bignum(&tmp);

	c->lastdigit = a->lastdigit;

	for (i=a->lastdigit; i>=0; i--) {
		digit_shift(&row,1);
		row.digits[0] = a->digits[i];
		c->digits[i] = 0;
		while (compare_bignum(&row,b) != PLUS) {
			c->digits[i] ++;
			subtract_bignum(&row,b,&tmp);
			row = tmp;
		}
	}

	zero_justify(c);

	a->signbit = asign;
	b->signbit = bsign;
}
void modulo(bignum *num, bignum *divisor, bignum *remainder)
{   bignum nx1,nx2;
    initialize_bignum(&nx1);
    initialize_bignum(&nx2);
    divide_bignum(num,divisor,&nx1);
    multiply_bignum(divisor,&nx1,&nx2);
    subtract_bignum(num,&nx2,remainder);

}
void power(bignum *base, bignum *exponent, bignum *result){
    bignum zero,one,nx1,nx2;
    int_to_bignum(0,&zero);
    int_to_bignum(1,&one);
    int_to_bignum(1,&nx2);
    nx1=*exponent;
while (compare_bignum(&zero, exponent) != 0) {
        multiply_bignum(&nx2,base,result);
        subtract_bignum(&nx1,&one,exponent);
        nx1=*exponent;
        nx2=*result;
    }
}

bignum gcd;
void gcdExtended(bignum a,bignum b,bignum *s,bignum *t)
{
     bignum zero,one;
    int_to_bignum(0,&zero);
    if(compare_bignum(&zero,&a) ==0){

    	int_to_bignum(1,&one);
    	*s = zero;
    	*t = one;
    	gcd = b;
        return b;
    }
    bignum x1,y1;

    bignum b1 = b;
    bignum a1 = a;
    bignum c;
    modulo(&b1,&a1,&c);
    gcdExtended(c,a,&x1,&y1);
    bignum b2 = b;
    bignum a2 = a;
    bignum temp1,temp2;
    divide_bignum(&b2,&a2,&temp2);
    multiply_bignum(&temp2,&x1,&temp1);
    subtract_bignum(&y1,&temp1,s);
    *t= x1;


}

bignum modInverse(bignum a,bignum m)
{
    bignum s,t,res;
    gcdExtended(a,m,&s,&t);
    bignum temp1,temp2;
    bignum m2 = m;
    modulo(&s,&m2,&temp2);
    add_bignum(&temp2,&m,&temp1);
    bignum m1 = m;
    modulo(&temp1,&m1,&res);
    return res;
}





bignum mb,xb,yb,gb,temp,pb,rb,c1,c2;


void decrypt() {
	bignum temp1,temp2,t;
	bignum xbTemp = xb;
	power(&c1,&xbTemp,&temp1);
	t = modInverse(temp1,pb);
	bignum ptemp = pb;
	multiply_bignum(&c2,&t,&temp2);
	bignum op;
	modulo(&temp2,&ptemp,&op);
	printf("Decrypted number :\n");
	print_bignum(&op);
	printf("\n");
}


void encrypt() {
        bignum randb,one,temp,c1temp,c2temp1,c2temp2;
        int ran = rand();
        int_to_bignum(ran,&randb);
        int_to_bignum(1,&one);
        subtract_bignum(&pb,&one,&temp);
        modulo(&randb,&temp,&rb);
        bignum rb1 = rb;
        power(&gb,&rb,&c1temp);
        power(&yb,&rb1,&c2temp1);
        modulo(&c1temp,&pb,&c1);
        multiply_bignum(&c2temp1,&mb,&c2temp2);
        modulo(&c2temp2,&pb,&c2);
        printf("Encrypted Number: \nc1=");
        print_bignum(&c1);
        printf("  c2=");
        print_bignum(&c2);
        printf("\n");
}

int main()
{
	int m;
	int p = 2111;
	int g = 211;
	int_to_bignum(g,&gb);
	printf("Please input a number (plaintext) :\n");
        scanf("%d",&m);
        bignum randb;
        int_to_bignum(m,&mb);
        int_to_bignum(p,&pb);
        int ran = rand();
        int_to_bignum(ran,&randb);
        modulo(&randb,&pb,&xb);
        bignum xbtemp = xb;
        power(&gb,&xbtemp,&temp);
        modulo(&temp,&pb,&yb);
	encrypt();
	decrypt();
	return 0;

}


