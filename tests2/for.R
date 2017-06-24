
library(RTypeInference)
library(R2llvm)
f =
function(n)
{
  total = 0L
  for(i in 1L:n)
      total = total + i
  return(total)
}
cfg = rstatic::to_cfg(f)
types = infer_types(cfg)
# , init = list(n = typesys::IntegerType()))

m = compile_cfg(f)


