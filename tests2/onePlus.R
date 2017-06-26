library(R2llvm)
f = function() 1L + 2L

#R2llvm recognizes this as a constant.

#debug(compileFunction)
f = compileFunction(f, optimize = FALSE)
