library(R2llvm)
library(RTypeInference)


# Set names
# convert list(1, 2) to create the SEXPs for the elements, i.e. know we are evaluating 1 and 2 in the context
# of a target type being a SEXP.

f =
function()
{
   x = numeric(2)  # try with 1 and make it work, i.e. do the casting..
   list(x)
#   list(integer(1), logical(2))
}

fc = compileFunction(f) # , .rewriteAST = FALSE)
ans = .llvm(fc)
stopifnot(is.list(ans) && length(ans) == 1L)


