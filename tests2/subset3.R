library(R2llvm)
library(RTypeInference)
f = function(x)
       x[1L]

cfg = rstatic::to_cfg(f)
types = RTypeInference::infer_types(cfg, init = list(x = ArrayType(BooleanType()))) 
#translate_type(typesys::ArrayType(typesys::BooleanType()))
#translate_type(types[[1]])
debug(createCast)
fc = compileFunction(f, cfg = cfg, types = types, .readOnly = "x")

stopifnot(identical(.llvm(fc, c(FALSE, TRUE)), 0L))
