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

#a1 = logistic_map(0.1, 3.6, 1e6L)    
.llvm(fc, 0.1, 3.6, 1e6L)
