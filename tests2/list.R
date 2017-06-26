library(R2llvm)
library(RTypeInference)

f =
function()
{
   list(1L, 2L)
}

compileFunction(f)

