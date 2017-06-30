library(RTypeInference)
library(R2llvm)
f =
function(n)
{
  total = 0L
  for(i in 1L:n)
      total = total + i
  total  
}

fc = compileFunction(f)

if(FALSE) {
N = 1e8
ee = ExecutionEngine(fc)
tm.llvm = system.time(.llvm(fc, N, .ee = ee))
tm.llvm = replicate(10, system.time(.llvm(fc, N, .ee = ee)))
tm.r = system.time(f(N))
tm.r[3]/median(tm.llvm[1,])
# For the integer version (total = 0L)
# On OSX laptop, speedup factor of 631 w/o the ExecutionEngin
# With the Execution engine preallocated, factor of 929.
#
# For the real-valued total (total = 0)
# Factor of 244.
# There is casting of i to a real value when adding it to total.
# See while2.R for a real-valued version of the counter.
#
}

