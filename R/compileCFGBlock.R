
compileCFGBlock =
function(block, types, ecompiler, builder, llvmFun, llvmBlocks = NULL)
{
  setInsertBlock(builder, llvmBlocks[[ block$name ]])
  
  if(length(block$phi)) 
     lapply(block$phi, compile, ecompiler, builder) # , types)
  
  lapply(block$body, compile, ecompiler, builder) # , types)
  if(length(block$terminator)) 
     constructTerminator(block$terminator, ecompiler, builder, llvmBlocks)
}






  
