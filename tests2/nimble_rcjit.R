# Needs default values to infer types at this point.
# However, just an example of a call, e.g. below, will do it.
logistic_map <- function(start = 1.0, scale = 1.0, steps = 1000L) {
    result <- start
    for (i in 1:steps) {
        start <- scale * result * (1.0 - result)
    }
    start
}


library(R2llvm)
fc = compileFunction(logistic_map)

a = logistic_map(0.1, 3.6, 1e6L)    
b = .llvm(fc, 0.1, 3.6, 1e6L)
stopifnot(identical(a,b))


if(FALSE) {
library(microbenchmark)
N = 1e6
mb = microbenchmark(.llvm(fc, 0.1, 3.6, N), logistic_map(0.1, 3.6, N), times = 20)
print(with(mb, do.call(`/`, rev(tapply(time, expr, summary)))))

ee = ExecutionEngine(fc)
# Warm this up with an initial call.
tm2 = system.time(a2 <- .llvm(fc, 0.1, 3.6, N, .ee = ee))
mb.ee = microbenchmark(.llvm(fc, 0.1, 3.6, N, .ee = ee), logistic_map(0.1, 3.6, N), times = 20)
print(with(mb.ee, do.call(`/`, rev(tapply(time, expr, summary)))))
}

