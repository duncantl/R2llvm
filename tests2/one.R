library(R2llvm)
f = function() 2L

#debug(compileFunction)
fc = compileFunction(f, optimize = FALSE)
stopifnot(identical(.llvm(fc), 2L))

