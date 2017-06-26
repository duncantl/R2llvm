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

constructBranch.CondBrTerminator =
function(term, cmp, helper, blocks)
{
  cond = construct_ir(term$condition, cmp, helper)
  helper$createCondBr( cond, blocks[[ term$true ]], blocks[[ term$false ]])
}
