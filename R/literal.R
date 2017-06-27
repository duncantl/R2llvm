setGeneric("asLiteral", function(x, ...) standardGeneric("asLiteral"))
setOldClass("Literal")
setMethod("asLiteral", "Literal", function(x, ...) x$value)
tmp = function(x, ...) x
setMethod("asLiteral", "numeric", tmp)
setMethod("asLiteral", "integer", tmp)
setMethod("asLiteral", "character", tmp)
setMethod("asLiteral", "logical", tmp)
