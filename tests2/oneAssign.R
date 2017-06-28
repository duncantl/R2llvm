# This shows that the optimizer will remove the assignment to a local variable.
library(R2llvm)
f = function(x = 1) {x = x + 1; x}
fc = compileFunction(f)
showModule(fc)

# Also, the CFG leaves the variable names unaltered (using the code in the duncan branch of rstatic)
rstatic::to_cfg(f)

