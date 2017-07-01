library(R2llvm)

f = function(x)
      x > 5L

cmpLogic =
function(f)
{
  cfg = rstatic::to_cfg(f)
  types = RTypeInference::infer_types(cfg, init = list(x = typesys::IntegerType()))
  fc = compileFunction(f, cfg, types)    
}

fc = cmpLogic(f)
stopifnot(.llvm(fc, 10) == 1)
stopifnot(.llvm(fc, 1) == 0)

#XX Remove this if()

f = function(x)
      x < 5L
fc = cmpLogic(f)
stopifnot(.llvm(fc, 10) == 0)
stopifnot(.llvm(fc, 1) == 1)


f = function(x)
      x == 5L
fc = cmpLogic(f)
stopifnot(.llvm(fc, 5) == 1)
stopifnot(.llvm(fc, 1) == 0)


f = function(x)
      x != 5L
fc = cmpLogic(f)
stopifnot(.llvm(fc, 5) == 0)
stopifnot(.llvm(fc, 1) == 1)



