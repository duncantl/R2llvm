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
  cond = compile(term$condition, cmp, helper) # , cmp$.types)
  helper$createCondBr( cond, blocks[[ term$true ]], blocks[[ term$false ]])
}
