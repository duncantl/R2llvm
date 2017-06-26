library(R2llvm)
f = function() numeric(10L)

fc = compileFunction(f)
