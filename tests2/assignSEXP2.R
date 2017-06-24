library(R2llvm)
library(rstatic)
library(RTypeInference)

f =
function()
{
   n = 2L
   x = numeric(n)
   x
}

cfg = to_cfg(f)
types = infer_types(cfg)
#m = compile_cfg(f)
