
compileCFGBlock =
function(block, types, ecompiler, builder, llvmFun, llvmBlocks = NULL)
{
  setInsertBlock(builder, llvmBlocks[[ block$name ]])
  lapply(block$body, construct_ir, ecompiler, builder, types)
  if(length(block$terminator)) 
     constructTerminator(block$terminator, ecompiler, builder, llvmBlocks)
}

constructTerminator =
function(term, cmp, helper, blocks)
  UseMethod("constructBranch")


constructBranch.BrTerminator =
function(term, cmp, helper, blocks)
  helper$createBr(blocks[[ term$dest ]])

constructBranch.RetTerminator =
function(term, cmp, helper, blocks)
{
  var = cmp$getAlloc(term$value$name) 
  helper$createRet(helper$createLoad(var))
}



  
