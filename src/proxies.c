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

int *
R_INTEGER(SEXP x)
{
    return(INTEGER(x));
}

void
R_protect(SEXP x)
{
    Rf_protect(x);
}

void
R_unprotect_ptr(SEXP x)
{
    Rf_unprotect_ptr(x);
}



#include <dlfcn.h>
void *RDLLHandle = NULL;
//XXX Should have an R_unload_R2llvm() routine that closes this handle we opened with dlopen().
SEXP
R_getRSymbol(SEXP r_name)
{
   if(!RDLLHandle)
       RDLLHandle = dlopen(NULL, RTLD_GLOBAL | RTLD_NOW);

   if(!RDLLHandle)   {
       PROBLEM "Can't use the dlopen() trick to get access to the routines in the R executable."
	   ERROR;
   }

   void *sym = dlsym(RDLLHandle, CHAR(STRING_ELT(r_name, 0)));
   if(!sym) {
       PROBLEM "cannot resolve native routine %s", CHAR(STRING_ELT(r_name, 0))
	   ERROR;
   }
   return(R_MakeExternalPtr(sym, Rf_install("dlsym"), R_NilValue));
}

#include <R_ext/Rdynload.h>
void
R_unload_R2llvm(DllInfo *dlinfo, char *buf, R_RegisteredNativeSymbol *sym)
{
    if(RDLLHandle) {
	dlclose(RDLLHandle);
	RDLLHandle = NULL;
    }
}
