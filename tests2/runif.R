library(R2llvm)

f = function(n)
      runif(n, 0, 100)

# This tries to compile runif() which we have to prohibit.
# infer_types() leaves no type for the n in runif().

types = RTypeInference::infer_types(f)

debug(compileFunction)
compileFunction(f, optimize = FALSE)
