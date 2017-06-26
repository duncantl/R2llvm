library(R2llvm)

f = function(x) 1L + x
cfg = rstatic::to_cfg(f, ssa = FALSE)
types = RTypeInference::infer_types(cfg, init = list(x = typesys::RealType()))
fc = compileFunction(f, cfg = cfg, types = types, optimize = FALSE)
stopifnot(identical(.llvm(fc, 2.5), 3.5))


f = function(x) 1.5 + x
cfg = rstatic::to_cfg(f, ssa = FALSE)
types = RTypeInference::infer_types(cfg, init = list(x = typesys::IntegerType()))
fc = compileFunction(f, cfg = cfg, types = types, optimize = FALSE)
stopifnot(identical(.llvm(fc, 2), 3.5))
