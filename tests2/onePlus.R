library(R2llvm)
f = function() 1L + 2L

#R2llvm recognizes this as a constant.

#debug(compileFunction)
fc = compileFunction(f, optimize = FALSE)
identical(.llvm(fc), 3L)
