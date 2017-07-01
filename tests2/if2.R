library(R2llvm)
f =
function(x)
{
   if(x > 10L)
      z = 20L
   else
      z = 1L
   return(z)
}

library(RTypeInference)
cfg = rstatic::to_cfg(f)
types = RTypeInference::infer_types(cfg, init = list(x = typesys::IntegerType()))
fc = compileFunction(f, cfg = cfg, types = types)
stopifnot(identical(.llvm(fc, 100), 20L))
stopifnot(identical(.llvm(fc, 1), 1L))

