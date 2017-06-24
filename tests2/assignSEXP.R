library(R2llvm)
library(rstatic)
library(RTypeInference)

f =
function()
{
   x = integer(2L) # numeric(2)
   x
}

cfg = to_cfg(f)
types = infer_types(cfg)
#m = compile_cfg(f)
