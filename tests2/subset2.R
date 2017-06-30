library(R2llvm)
library(RTypeInference)
f = function(x)
       x[1L]

cfg = rstatic::to_cfg(f)
types = RTypeInference::infer_types(cfg, init = list(x = ArrayType(IntegerType()))) 
fc = compileFunction(f, cfg = cfg, types = types, .readOnly = "x")
llvmAddSymbol("R_INTEGER")

stopifnot(identical(.llvm(fc, c(101L, 20L)), 101L))
