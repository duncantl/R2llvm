library(RLLVMCompile)

f =
function(x)
{
  ctr = 0
  for(i in x)
      ctr = ctr + i
  
  ctr
}

fc = compileFunction(f, DoubleType, list(REALSXPType))

.llvm(fc, as.numeric(1:10))
m = as(fc, "Module")
#createGlobalVariable("num", m, Int32Type, 10L)

ir = showModule(m, TRUE)

save(ir, file = "ir.rda")
rm(m)

#Rllvm::shutdown()
#InitializeNativeTarget()

