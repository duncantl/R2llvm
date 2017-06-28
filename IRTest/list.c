#include <Rdefines.h>

SEXP
foo()
{
    SEXP x = NEW_NUMERIC(10);
    PROTECT(x);
    SEXP l = NEW_LIST(1);
    PROTECT(l);
    SET_VECTOR_ELT(l, 0, x);
    UNPROTECT(2);
    return(l);
}
