library(R2llvm)
f = function(x) {
       x[1L] = 10.4  # TRY WITH 10 since that will coerce down.
       x
   }

library(RTypeInference)
cfg = rstatic::to_cfg(f, ssa = FALSE)
types = RTypeInference::infer_types(cfg, init = list(x = ArrayType(RealType()))) # REALSXPType))
fc = compileFunction(f, cfg = cfg, types = types, .readOnly = "x")

stopifnot(identical(.llvm(fc, c(101, 20)[1]), 10.4))

