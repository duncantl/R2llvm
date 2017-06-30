
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
ast = rstatic::to_ast(f)
cfg = rstatic::to_cfg(ast)
types = infer_types(cfg)
# , init = list(n = typesys::IntegerType()))

#m = compilefFunction(f)


