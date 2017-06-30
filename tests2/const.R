library(R2llvm)
f = function() 2
fc = compileFunction(f)
stopifnot(identical(.llvm(fc), 2))

