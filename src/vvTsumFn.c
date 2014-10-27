#include <R.h>
#include <stdio.h>
#include <math.h>


void vvTsumFn(double *uuu, int *nrow, int *ncol, double *adder, double *mult) 
{
	int i, j;
	
	double row[*ncol]; 
	
	for (int n = 0; n < *nrow; n++) {
		for (i = 0; i < *ncol; i++) row[i] = uuu[ n * *ncol + i];
		
		for (i = 0; i < *ncol; i++) {
			for (j = 0; j < *ncol; j++) {
				// RHS is u %*% t(u) * a
				// We are iteratively adding it to 'adder'
				adder[i * *ncol + j] += row[i] * row[j] * mult[n];	
			}
		}
	}
}



































/*
void vvTsumFn(double *uuu, int *nrow, int *ncol, double *adder, double *mult)
{
	double u_uT_a[*ncol * *ncol];
	int i, j;
	
	double row[*ncol]; 
	
	for (int n = 0; n < *nrow; n++) {
		
		// grab the person's uuu (a row from uuu)	
		for (i = 0; i < *ncol; i++) row[i] = uuu[*ncol * n + i];
		
		
		Rprintf("Row: %d \n", n);
		for (i = 0; i < *ncol; i++) Rprintf(" %.2f", row[i]);
		Rprintf("\n \n");
		
		
		// define u_uT_a = u * uT * mult 
		for (i = 0; i < *ncol; i++) {
			for(j = 0; j < *ncol; j++) {
				u_uT_a[*ncol * i + j] = row[i] * row[j] * mult[n];
			}
		}
		
		// add current u_uT_a to adder
		for (i = 0; i < (*ncol * *ncol); i++)	adder[i] += u_uT_a[i];
	}
}
*/
