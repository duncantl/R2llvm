library(rstatic)
library(RTypeInference)

foo =
function(x = 1L)
{
    return(x + 2L)
}

minus =
function(x = 1L)
{
    return(x - 2L)
}

cfg = to_cfg(foo)
types = infer_types(cfg)

library(R2llvm)
f = compileFunction(foo, cfg, types)
mod = as(f, "Module")

Rllvm::.llvm(f, 1L)
Rllvm::.llvm(f, 10L)

m = compileFunction(minus, module = mod)

names(mod)
showModule(mod)

ans = Rllvm::.llvm(m, 10L)
stopifnot(identical(ans, 8L))



