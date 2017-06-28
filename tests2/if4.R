library(R2llvm)
f =
function(x)
{
   if(x > 10L) {
      a = 1L + x
      z = a + 20L
   } else {
      z = 1L
   }
   return(z)
}

library(RTypeInference)
cfg = rstatic::to_cfg(f)#, ssa = FALSE)
types = RTypeInference::infer_types(cfg, init = list(x = typesys::IntegerType()))
#fc = compileFunction(f, cfg = cfg, types = types)

