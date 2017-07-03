library(R2llvm)
f = function(x) {
       x[1L] = 10.4  # TRY WITH 10 since that will coerce down.
       x
   }

library(RTypeInference)
#XXXXX NEED TO DO THIS WITH ssa = TRUE
cfg = rstatic::to_cfg(f, ssa = FALSE)
cfg = rstatic::to_cfg(f) 
types = RTypeInference::infer_types(cfg, init = list(x = ArrayType(RealType()))) # REALSXPType))
#fc = compileFunction(f, cfg = cfg, types = types, .readOnly = "x")
fc = compileFunction(f, cfg, types = types)

stopifnot(identical(.llvm(fc, c(101, 20)), c(10.4, 20)))


