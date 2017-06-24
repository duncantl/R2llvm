library(R2llvm)
library(rstatic)
library(RTypeInference)

f =
function()
{
   x = 2L
   x
}

cfg = to_cfg(f)
types = infer_types(cfg)
#m = compile_cfg(f)
