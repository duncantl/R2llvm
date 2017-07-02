#
# The following is from https://github.com/nimble-dev/rcjit, NIMBLE's R early-stage jit compiler
#
# Summary:
# NIMBLE's rcjit currently gets a speed up for the following R function (logistic_map())
# of about 16.5 on one particular machine (which runs the R code 3 times slower than this machine).
#
# We compile the logistic_map() function using RLLVMCompile. This gives us a speed up over the R
# interpreter of a factor of about 49.
# When we create the ExecutionEngine() ahead of time and pass that to the .llvm() call,
# we get a speedup factor of 230. 
#
# The following is on OSX with LLVM 3.8
#

logistic_map <- function(start, scale, steps) {
    result <- start
    for (i in 1:steps) {
        start <- scale * result * (1.0 - result)
    }
    start
}
N = 1e6L
tm = system.time(a1 <- logistic_map(0.1, 3.6, N))


    # old RLLVMCompile so we specify the parameter types and it tries to figure out the rest.
library(RLLVMCompile)
z = compileFunction(logistic_map, DoubleType, list(DoubleType, DoubleType, Int32Type))
tm2 = system.time(a2 <- .llvm(z, 0.1, 3.6, N))

stopifnot(identical(a1, a2))

# For N = 1e6 about 16
#tm/tm2
#    user   system  elapsed 
#47.77778 17.00000 42.38095

library(microbenchmark)
# Benchmark the two without an ExecutionEngine
mb = microbenchmark(.llvm(z, 0.1, 3.6, N), logistic_map(0.1, 3.6, N), times = 20)
print(with(mb, do.call(`/`, rev(tapply(time, expr, summary)))))

# A factor of 49 - 56 speedup.

ee = ExecutionEngine(z)
# Warm this up with an initial call.
tm2 = system.time(a2 <- .llvm(z, 0.1, 3.6, N, .ee = ee))
mb.ee = microbenchmark(.llvm(z, 0.1, 3.6, N, .ee = ee), logistic_map(0.1, 3.6, N), times = 20)
print(with(mb.ee, do.call(`/`, rev(tapply(time, expr, summary)))))

# Basically seeing about a factor of 230 - 260 speedup 




llvmVersion()
sessionInfo()

