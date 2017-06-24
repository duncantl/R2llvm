logistic_map <- function(start, scale, steps) {
    result <- start
    for (i in 1:steps) {
        result <- scale * result * (1.0 - result)
    }
    return(result)
}
tm = system.time(logistic_map(0.1, 3.6, 1000000))



