library(R2llvm)
f = function() "abc"
fc = compileFunction(f)
