library(R2llvm)
f = function() {
     x = numeric(10L)
     Rf_protect(x)
     Rf_PrintValue(x)
     Rf_unprotect(1L)
     x
 }
fc = compileFunction(f)
#llvmAddSymbol("Rf_allocVector")

showModule(fc)
#gctorture(TRUE)
x = .llvm(fc)
#gctorture(FALSE)
# Why not identical to 0

