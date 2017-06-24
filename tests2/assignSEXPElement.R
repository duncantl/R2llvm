library(R2llvm)
library(rstatic)
library(RTypeInference)

f =
function()
{
   x = integer(2L)
   x[1] = 2L
   x[2] = 10L   
   x
}

cfg = to_cfg(f)
types = infer_types(cfg)
#m = compile_cfg(f)
