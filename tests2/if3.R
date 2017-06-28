library(R2llvm)
f =
function(x)
{
#   y = x + 1L
   if(x > 10L)
      z = 20L
   else if(x > 0L)
      z = 5L
   else
      z = 1L
   return(z)
}

library(RTypeInference)
cfg = rstatic::to_cfg(f)#, ssa = FALSE)
types = RTypeInference::infer_types(cfg, init = list(x = typesys::IntegerType()))
fc = compileFunction(f, cfg = cfg, types = types)
#.llvm(fc, 5)

