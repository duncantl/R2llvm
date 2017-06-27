
subsetDoubleHandler =
function(call, env, ir, ..., load = TRUE, SEXPToPrimitive = TRUE, .targetType = NULL)
{
  if(is.numeric(call[[3]]))
      call[[3]] = as.integer(call[[3]])
  e = substitute(VECTOR_ELT(x, i), list(x = call[[2]], i = subtractOne(call[[3]])))
  compile(e, env, ir, ...)
#  objType = getElementAssignmentContainerType(call, env)
#  index = compile(call[[3]], env, ir, ...)
  
}
    
subsetHandler =
#
# Attempt to handle subsetting a vector of any type.
#
#  Need to make this understand SEXP types and need to differentiate between expecting it back as a native element or a SEXP with one element.
#
#  Also handle vector subsetting.  
#
# References:
#  - GEP: http://llvm.org/docs/LangRef.html#i_getelementptr
#  - SExt: http://llvm.org/docs/LangRef.html#i_sext
function(call, env, ir, ..., objType = getElementAssignmentContainerType(call, env), load = TRUE, SEXPToPrimitive = TRUE, .targetType = NULL)
{
browser()  

    if(is(call, "Call")) {
        op = call$fn$name
        args = call$args
    } else {
        op = as.character(call[[1]])
        args = as.list(call[-1])
    }

  if(length(args) >= 3)
                    # perhaps make this a separate method and have the generic dispatcher call it.
      return(multiSubset(call, env, ir, ..., load = load, SEXPToPrimitive = SEXPToPrimitive))


  if(is(objType, "SEXPType")) {  # is this already in compile.=? If so, consolidate.
    if(SEXPToPrimitive) {
      r = getSEXPTypeElementAccessor(objType)
      declareFunction(env$.builtInRoutines[[r]], r, env$.module)

# Could we just compile the call to ,e.g., REAL() and then do the getelementptr.
       # Basically creating, e.g.,
         #  .tmp = REAL(x)
         #  .tmp[i]
         # This is to avoid duplicating code elsewhere.
      e1 = substitute(.tmp <- f(x), list(f = as.name(r), x = asRCall(args[[1]])))
      e2 = substitute(.tmp[i], list(i = asRCall(args[[2]])))

      if(length(env$.loopStack)) {
          # This is for cases such as for() {  .tmp = REAL(x) ; .tmp[i] } where we added the .tmp = REAL() into 
          # We can  lift this accessor to the entry block and create as a non-loop local variable
          # If this is in a nested loop, we have to be careful in case the variable is loop-local
          # i.e. in the parent loop.
          # If we are subsetting a parameter, no problem, as long as it is a simple x[].
          # If it is x[i][j] then we have to be more careful.
          #

#!!!XXXX This seems like a lot of bookkeeping. Can we insert the new instruction just before the terminator.          
          cur = getInsertBlock(ir)
                # Need to put new code before the existing terminator in the entry block.
#XXX FIX Should be the block before the           
          term = getTerminator(env$.entryBlock)  #XXX is this always the entry block????
          eraseFromParent(term, FALSE)
          setInsertBlock(ir, env$.entryBlock)
          tmpVarName = e2[[2]] = e1[[2]] =  as.name(sprintf("%s.%s", r, as.character(args[[1]]))) # make a fake name
          compile(e1, env, ir)
          insertAtEnd(term, env$.entryBlock)
          setInsertBlock(ir, cur)
      } else 
          compile(e1, env, ir)
      
      return(compile(e2, env, ir))

    } else
       stop("subsetting SEXPs as SEXPs is not implemented yet")
    }

    var = args[[1]]
    ridx = args[[2]]

 
        # ty = getDataType(obj, env)
     # do we need to load this.  The compile.= function 
  obj = getVariable(var, env, ir, load = TRUE) #???? for load = FALSE. Now back to TRUE. Based on fgets.Rdb.

    #XXX Need to handle subsetting generally and need to ensure we get an integer

  zeroBased = is.name(ridx) && as.character(ridx) %in% names(env$.zeroBased)


  if(!zeroBased)
     ridx = subtractOne(ridx)

  i = compile(ridx, env, ir, isSubsetIndex = TRUE) # getVariable(call[[3]], env, ir)
  #i = getVariable(call[[3]], env, ir)
  idx = ir$createSExt(i, 64L)

  ty = getType(obj)
  if(isArrayType(ty)  || 
         (isPointerType(ty) && isArrayType(getElementType(ty))))
    idx = list(createIntegerConstant(0L, getContext(env$.module)), idx)

  if(!isPointerType(ty) && !isArrayType(ty))
      stop("attempting to create a GEP for a non-pointer type")

  p = ir$createGEP(obj, idx)

  if(load)
    return(ir$createLoad(p))
  
  return(p)
}


subtractOne =
function(k)
{  
 if(is(k, "numeric"))
    return(as.integer(k - 1L))

  tmp = quote(a - 1L)
  tmp[[2]] = k
  tmp
}


subsetAssignHandler =
  #
  # Never used!  Use compile.<- in compile.R
  # It may be good to split that part out to element assignment but okay for now.
  #
function(call, env, ir, ..., .targetType = NULL)
{
  ll = subsetHandler(call, env, ir, load = FALSE)
#  ir$createStore(tmp, ans.i)
}
  
