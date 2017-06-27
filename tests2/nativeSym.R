library(R2llvm)
as("Rf_protect", "NativeSymbol")

# Check when no such symbol is available that we get an error.
stopifnot(identical(tryCatch(as("xRf_protect", "NativeSymbol"), error = function(e) "failed"), "failed"))
