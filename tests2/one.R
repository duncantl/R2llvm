library(R2llvm)
f = function() 2L

#debug(compileFunction)
f = compileFunction(f, optimize = FALSE)
