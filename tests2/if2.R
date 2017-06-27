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
cfg = rstatic::to_cfg(f, ssa = FALSE)
types = RTypeInference::infer_types(cfg, init = list(x = IntegerType()))

fc = compileFunction(f, cfg = cfg, types = types)

