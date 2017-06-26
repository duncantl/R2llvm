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

