library(R2llvm)
# infer_types can't handle global variables yet.
f = function() 1 + pi

#R2llvm should recognize this as a constant.

f = compileFunction(f, optimize = FALSE)
