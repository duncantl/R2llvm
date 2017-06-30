# This is a while/for loop very similar to that in for.R
# but with the types of the counter and upper limit of the loop being RealType
# This avoids any casting. But we are doing more floating point operations.
# Compare to the for.R code.
f =
function(n)
{
   total = 0
   i = 1
   while(i <= n) {
       total = total + i
       i = i + 1
   }
   i
}

library(R2llvm)
cfg = rstatic::to_cfg(f)
types = RTypeInference::infer_types(cfg, init = list(typesys::RealType()))
fc = compileFunction(f, cfg, types, optimize = TRUE)
showModule(fc)
N = 1e8
ee = ExecutionEngine(fc)
system.time(.llvm(fc, N, .ee = ee))
tm.llvm.while = replicate(10, system.time(.llvm(fc, N, .ee = ee)))
# Compare with the results in for.R.
