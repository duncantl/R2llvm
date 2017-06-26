construct_ir = function(node, cmp, helper, types) 
  UseMethod("construct_ir")


construct_ir.BranchInst =
function(node, cmp, helper, types) {
  # Connect block to its successors.
  # FIXME: Check condition on branch instruction.
  helper$createBr(node$true)
}

construct_ir.ReturnInst =
function(node, cmp, helper, types) {
  # FIXME: What if retval is not a symbol?
  alloc = helper$get_alloc(node$value$name)
  val = helper$create_load(alloc)
  helper$create_ret(val)
}

construct_ir.Assign =
function(node, cmp, helper, types)
{
  # Allocate memory for RHS.
  type = types[[node$write$name]]

  lhs = helper$createAlloc(type, id = node$write$name)
  cmp$newAlloc(node$write$name, lhs)

  # Store LHS into RHS.
  rhs = construct_ir(node$read, cmp, helper, types)

  # Check the types match
  rtype = Rllvm::getType(rhs)
  if(!sameType(rtype, type)) 
    rhs = createCast(cmp, helper, type, rtype, rhs)

  helper$createStore(rhs, lhs)

  lhs
}

construct_ir.Call =
function(node, cmp, helper, types)
{
  # FIXME: What if $fn is not a symbol?
  idx = match(node$fn$name, names(cmp$.compilerHandlers))
  if (is.na(idx)) 
     compile.call(node, cmp, helper)
  else 
    cmp$.compilerHandlers[[idx]](node, cmp, helper)
}

construct_ir.Phi =
function(node, cmp, helper, types) {
browser()    
  # Don't insert phis for shadowed globals.
  if ( any(is_global(node$read)) )
    return (NULL)

  type = types[[node$write$name]]
  helper$create_phi(type, node$write$name)

  #Map(function(b, name) {
  #  llvm_block = helper$get_llvm_block(b)
  #  val = helper$get_alloc(name)
  #  Rllvm::addIncoming(phi, val, llvm_block)
  #}, as.integer(names(node$read)), node$read)
}

construct_ir.Symbol =
function(node, cmp, helper, types)
{
  v = cmp$getAlloc(node$name)
  if(is.null(v))
     v = cmp$.params[[ node$name ]]
  v  
#  helper$createLoad(  )
}

construct_ir.Integer =
function(node, cmp, helper, types)
  createIntegerConstant(node$value)

construct_ir.BrTerminator =
function(node, cmp, helper, types) 
   helper$createBr(node$dest)


construct_ir.RetTerminator =
function(node, cmp, helper, types) 
   Rllvm::createReturn(helper$builder, Rllvm::createLoad(helper$builder, helper$alloc_table[["._return__1"]]))


construct_ir.default =
function(node, cmp, helper, types) 
  browser()

construct_ir.Numeric =
function(node, cmp, helper, types)
   createConstant(helper, node$value)

