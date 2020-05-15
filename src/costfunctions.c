#include <R.h>
#include <Rmath.h>
#include <Rinternals.h> // RK addition
#include <R_ext/RS.h>  // RK addition
#include <R_ext/Lapack.h> // RK addition
#include <R_ext/BLAS.h> // RK addition
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#define SWAP(a,b)   { int t; t=a; a=b; b=t; }  // Macro for swapping

// Cost functions
double mll_nonparametric_ed(double sumstatout[], int tstar, int checklist, int *nquantiles, int *n){
  double Fkl;
  double temp_cost;
  double cost;
  int nseg, isum;

  cost = 0;
  temp_cost = 0;
  nseg = tstar - checklist;

  for(isum = 0; isum < *nquantiles; isum++){
    Fkl = (sumstatout[isum])/(nseg);
    temp_cost = (tstar-checklist)*(Fkl*log(Fkl)+(1-Fkl)*log(1-Fkl));
    if(!isnan(temp_cost)){
      cost = cost + temp_cost;
    }
    //else{
    //  cost = cost;
    //}
  }
  cost = -2*(log(2**n-1))*cost/(*nquantiles);
  return(cost);
}

double mll_nonparametric_ed_mbic(double sumstatout[], int tstar, int checklist, int *nquantiles, int *n){
  double Fkl;
  double temp_cost;
  double cost;
  int nseg, isum;

  cost = 0;
  temp_cost = 0;
  nseg = tstar - checklist;

  for(isum = 0; isum < *nquantiles; isum++){
    Fkl = (sumstatout[isum])/(nseg);
    temp_cost = (tstar-checklist)*(Fkl*log(Fkl)+(1-Fkl)*log(1-Fkl));
    if(!isnan(temp_cost)){
      cost = cost + temp_cost;
    }
    //else{
    //  cost = cost;
    //}
  }
  cost = -2*(log(2**n-1))*cost/(*nquantiles);
  return(cost);
}


void min_which(double *array,int n,double *minout,int *whichout){
  // Function to find minimum of an array with n elements that is put in min
  *minout=*array;
  *whichout=0;
  int i;
  for(i=1;i<n;i++){
    if(*(array+i)< *minout){
      *minout= *(array+i);
      *whichout=i;
    }
  }
}

void order_vec( int a[], int n ){
  int i, j;
  for(i = 0; i < n; i++){         // Make a pass through the array for each element
    for(j = 1; j < (n-i); j++){  		// Go through the array beginning to end
      if(a[j-1] > a[j])       // If the the first number is greater, swap it
        SWAP(a[j-1],a[j]);
    }
  }
}
