library(R2llvm)
f = function(x)
       x[1L]

library(RTypeInference)
cfg = rstatic::to_cfg(f)
#debug(RTypeInference:::solve.ConstraintSet)
types = RTypeInference::infer_types(cfg, init = list(x = ArrayType(RealType()))) # REALSXPType))
fc = compileFunction(f, cfg = cfg, types = types, .readOnly = "x", optimize = FALSE)

stopifnot(identical(.llvm(fc, c(101, 20)), 101))


#########

message("ArrayType[Integer]\n")
types = RTypeInference::infer_types(cfg, init = list(x = ArrayType(IntegerType()))) 
fc = compileFunction(f, cfg = cfg, types = types, .readOnly = "x")



stopifnot(identical(.llvm(fc, c(101L, 20L)), 101L))


############
# Logical

types = RTypeInference::infer_types(cfg, init = list(x = ArrayType(BooleanType()))) 
#translate_type(typesys::ArrayType(typesys::BooleanType()))
translate_type(types[[1]])
fc = compileFunction(f, cfg = cfg, types = types, .readOnly = "x")

stopifnot(identical(.llvm(fc, c(FALSE, TRUE)), 0L))



