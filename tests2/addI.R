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
m = compile_cfg( , "foo", cfg, types)

Rllvm::.llvm(m$foo, 1L)
Rllvm::.llvm(m$foo, 10L)

m = compile_cfg(minus, mod = m)

names(m)
ans = Rllvm::.llvm(m$minus, 10L)
stopifnot(ans == 8)



