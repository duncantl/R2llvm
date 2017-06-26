
compileCFGBlock =
function(block, types, ecompiler, builder, llvmFun, llvmBlocks = NULL)
{
  setInsertBlock(builder, llvmBlocks[[ block$name ]])
  lapply(block$body, construct_ir, ecompiler, builder, types)
  if(length(block$terminator)) 
     constructTerminator(block$terminator, ecompiler, builder, llvmBlocks)
}






  
