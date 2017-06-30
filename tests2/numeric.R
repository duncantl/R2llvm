library(R2llvm)
f = function() numeric(10L)
fc = compileFunction(f)
#llvmAddSymbol("Rf_allocVector")

showModule(fc)
.llvm(fc)
# Why not identical to 0

