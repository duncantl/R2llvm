f =
function()
{
   i = 0L
   while(i < 10L) {
       i = i + 1L
   }
   i
}

library(R2llvm)
fc = compileFunction(f, optimize = TRUE)
showModule(fc)
stopifnot(identical(.llvm(fc), 10L))


