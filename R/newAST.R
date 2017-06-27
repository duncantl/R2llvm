setOldClass("Symbol")
setOldClass("Literal")
setOldClass("Call")
setOldClass(c("Integer", "Literal"))
setOldClass(c("Numeric", "Literal"))


setGeneric("asRCall", function(x, ...) standardGeneric("asRCall"))

setMethod("asRCall", "Symbol",
          function(x)
             as.name(x$name))

setMethod("asRCall", "Literal",
          function(x)
             x$value)

setMethod("asRCall", "Call",
          function(x){
        e = substitute( x(), list(x = asRCall(x$fn)))
        if(length(x$args)) {
           a = lapply(x$args, asRCall)
           e[seq(along = a) + 1L] = a
        }
        e
      })



setMethod("asRCall", "Assign",
          function(x) {
             e = quote(x <- v)
             e[[2]] = asRCall(x$write)
             e[[3]] = asRCall(x$read)             
             e
          })


setMethod("asRCall", "Replacement",
         function(x) {
             e = quote(.tmp[] <- 1)
             e[[3]] = asRCall(x$args[[length(x$args)]])
             e[[1]] = as.name("=")
             
             a= call("[", as.name(".tmp"))
             a[[2]] = asRCall(x$args[[1]])
             idx = x$args[ - unique(c(1, length(x$args) )) ]
             ridx = lapply(idx, asRCall)
             a[seq(along = ridx) + 2L] = ridx
             e[[2]] = a
             e
         }

          )
