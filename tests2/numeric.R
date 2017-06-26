library(R2llvm)
f = function() numeric(10L)
fc = compileFunction(f)
llvmAddSymbol("R_allocVector")

showModule(fc)
.llvm(fc)
# Why not identical to 0
