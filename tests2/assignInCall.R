library(rstatic)

to_astq(foo( x <- 1:10))
#<Call> $args $fn $parent $copy() $set_args() $set_fn()
#foo((x = 1:10))
#
# The  name is in the assignment which is within a Brace/paren
# The call has one $arg which is an Assign.

f =
function()
{
  foo(x <- 1:10)
  x
}
#source("~/NickUThesis/rstatic/R/ASTTraverse.R");
source("../R/rewrite.R");
a = rewriteAST(f)

f =
function()
{
  foo(2, 5, x <- 1:10)
  x
}
b = rewriteAST(f)


# This is an example of what people shouldn't do
# Assigning in a function and then never using the variable.
# Maybe a reason why we should work on the Call and look for Assign's in it is args.
f =
function()
 foo(x <- 1:10)

c = rewriteAST(f)


#rewriteAST(to_astq( foo(x <- 1:10)))

# Result shows up 

