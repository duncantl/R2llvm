construct_ir =
function(node, cmp, helper,  types, .targetType = NULL)
{
  UseMethod("construct_ir")
}


construct_ir.BranchInst =
function(node, cmp, helper,  types, .targetType = NULL) {
  # Connect block to its successors.
  # FIXME: Check condition on branch instruction.
  helper$createBr(node$true)
}

construct_ir.ReturnInst =
function(node, cmp, helper,  types, .targetType = NULL) {
  # FIXME: What if retval is not a symbol?
  alloc = helper$get_alloc(node$value$name)
  val = helper$create_load(alloc)
  helper$create_ret(val)
}

construct_ir.Assign =
function(node, cmp, helper,  types, .targetType = NULL)
{

    # For dealing with assignments that are actually for Phi nodes.
if(node$write$name %in% names(cmp$.phiVarInstructions)) {
    .targetType = cmp$.types[[node$write$name]]
    i = compile(node$read, cmp, helper, .targetType = .targetType)
    cmp$.phiVarInstructions[[node$write$name]] = i
    return(i)
}

   call = asRCall(node)
   return(`compile.=`(call, cmp, helper, .targetType = .targetType))
    
  # Allocate memory for RHS.
  type = types[[node$write$name]]

  lhs = helper$createAlloc(type, id = node$write$name)
  cmp$newAlloc(node$write$name, lhs)

  # Store LHS into RHS.
  rhs = construct_ir(node$read, cmp, helper,  types, .targetType = NULL)

#Hack for now to avoid bad cast.  
if(node$write$name != "._return_") {  
  # Check the types match
  rtype = Rllvm::getType(rhs)
  if(!sameType(rtype, type)) 
    rhs = createCast(cmp, helper, type, rtype, rhs)
}

  helper$createStore(rhs, lhs)

  lhs
}

construct_ir.Call =
function(node, cmp, helper,  types, .targetType = NULL)
{
  # FIXME: What if $fn is not a symbol?
  idx = match(node$fn$name, names(cmp$.compilerHandlers))
  if (is.na(idx)) 
     compile.call(node, cmp, helper)
  else 
    cmp$.compilerHandlers[[idx]](node, cmp, helper)
}

construct_ir.Phi =
function(node, cmp, helper,  types, .targetType = NULL)
{
    
  # Don't insert phis for shadowed globals.
#XXXX
#  if ( any(is_global(node$read)) )
#    return (NULL)

  type = types[[node$write$name]]
  numIncoming = length(node$blocks)
  phi = Rllvm::createPhi(helper, type, numIncoming, id = node$write$name)
# Add the incoming. We have them in the node$blocks and in node$read

  mapply(function(var, block) {
           val = cmp$.phiVarInstructions[[ var$name ]]
# Are we supposed to createLoad(val). Get a "Instruction does not dominate all uses!" error.
           addIncoming(phi, val, block)
         }, node$read, cmp$blocks[node$blocks])

  if(node$write$name %in% names(cmp$.phiVarInstructions)) 
     cmp$.phiVarInstructions[[ node$write$name ]] <- phi
  else
     cmp$.allocVars[[ node$write$name ]] <- phi

#  e = substitute(x <- y, list(x = as.name(node$write$name), y = phi))
#  v = `compile.=`(e, cmp, helper)
  phi
}

construct_ir.Symbol =
function(node, cmp, helper,  types, .targetType = NULL)
{
#browser()    
  v = cmp$getAlloc(node$name)
  if(is.null(v)) {
     v = cmp$.params[[ node$name ]]
     return(v)
  }
#  v  
  helper$createLoad( v )
}

construct_ir.Integer =
function(node, cmp, helper,  types, .targetType = NULL)
  createIntegerConstant(node$value)

construct_ir.BrTerminator =
function(node, cmp, helper,  types, .targetType = NULL) 
   helper$createBr(node$dest)


construct_ir.RetTerminator =
function(node, cmp, helper,  types, .targetType = NULL) 
   Rllvm::createReturn(helper$builder, Rllvm::createLoad(helper$builder, helper$alloc_table[["._return__1"]]))


construct_ir.default =
function(node, cmp, helper,  types, .targetType = NULL) 
  browser()

construct_ir.Numeric =
function(node, cmp, helper,  types, .targetType = NULL)
{
browser()    
   createConstant(helper, node$value)
}


construct_ir.Replacement =
function(node, cmp, helper,  types, .targetType = NULL)
{
 return(`compile.=`(node, cmp, helper))

 
# idx = match(node$fn$name, names(cmp$.compilerHandlers))
# if (is.na(idx)) 
#    compile(node, cmp, helper)
# else 
#   cmp$.compilerHandlers[[idx]](node, cmp, helper)    
}


