#include <Rdefines.h>


SEXP
R_allocVector(int type, int num)
{
    Rprintf("In R_allocVector %d %d\n", type, num);
    return(Rf_allocVector(type, num));
}

double *
R_REAL(SEXP x)
{
    return(REAL(x));
}
