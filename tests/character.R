library(RLLVMCompile)
readTo =
function()
{
   tmp = "foo"
   1L
}

#fun = compileFunction(readTo, StringType)
fun = compileFunction(readTo, Int32Type, optimize = FALSE)
showModule(fun)
.llvm(fun)






