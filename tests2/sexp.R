library(R2llvm)
f = function(x) x
cfg = rstatic::to_cfg(f) 
types = RTypeInference::infer_types(cfg, init = list(x = ArrayType(RealType()))) # REALSXPType))
fc = compileFunction(f, cfg = cfg, types = types, .readOnly = "x")

showModule(fc)
orig = c(1, 20)
stopifnot(identical(.llvm(fc, orig), orig))
