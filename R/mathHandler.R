mathHandler =
  #
  # This currently (Feb 7, 2pm) attempts to do a little bit
  # of figuring about the types of the two operands.
  # It just handles integer and numeric types for now.
  #
  #  This has to handle both binary and unary calls, e.g. - x
  #
  #
  #
function(call, env, ir, ..., isSubsetIndex = FALSE, .targetType = NULL)  
{
# if(FALSE && length(call) == 2)  {
#    # unary operator - most likely -
#    val = compile(call[[2]], env, ir, ...)
#
#    ty2 = getTypes(call[[2]], env)
#    ## TODO fix ir$ SS, clean this
#    if (identical(ty2, Int32Type)) {
#      return(createNeg(val))
#       # return(createNeg(ir, val, call[[2]]))  #?? as.character(call[[2]])))     
#    }
#    
#    if (identical(ty2, DoubleType)) {
#      return(createFNeg(ir, val, as.character(call[[2]])))
#    }
#
#    stop("cannot createNeg for this type yet.")
# }


    if(is(call, "Call")) {
        op = call$fn$name
        args = call$args
    } else {
        op = as.character(call[[1]])
        args = as.list(call[-1])
    }
    
browser()

    
  if(length(args) == 1) {  # So a unary operation

       #XXX temporary exploration
        # if this is +, e.g. +n, we should just compile call[[2]]
     if(op == "+")
        return(construct_ir(args[[1]], env, ir, env$.types)) # ..., isSubsetIndex = isSubsetIndex))

      # XXX what about !
     k = quote(0 - 0)
     k[[3]] = args[[1]]
     k[[1]] = as.name(op)
     call = k
 }  # Finished with unary operation


  args = lapply(args, rewriteExpressions, env, isSubsetIndex = isSubsetIndex)

  origCall = call

  lit <- sapply(args, function(x) is.numeric(x) || is(x, "Integer") || is(x, "Numeric")) # literals

  if(all(lit)) {
          # The two operands are literal values so no need to compile run-time code.
          # So compute the result here and return.
    value = do.call(get(op), lapply(args, asLiteral)) # function(x) x$value))
    if(is.numeric(value) && env$.useFloat)
       return(createFloatingPointConstant(value, getContext(env$.module), FloatType))
    else
       return(createConstant(, value, context = getContext(env$.module)))
  }    

  
   # Need to handle the case where we have a literal and we can convert it to the
   # target type.
  toCast = NULL


     # we are getting the types here w/o compiling the expressions (?). So they may not be what we end up with.
  types = lapply(args, getTypes, env)
#XXX FIX    
  if(any(nulls <- sapply(types, is.null))) {
     i = which(nulls) + 1L
     call[i] = lapply(call[i], compile, env, ir, ...)
     types[nulls] = lapply(call[-1][nulls], getType)
#     stop("NULL value for component type in math operation")
  }


  targetType = getMathOpType(types)

  if(any(lit) && length(unique(types)) == 2) {
    # This has the problem that the literal will be coerced to the
    # other type, a non-R behavior. TODO remove entirely?
    ## targetType = getTypes(as.list(call[-1])[!lit][[1]], env)
    
  #  targetType = getMathOpType(types[!lit])
    toCast = lit
  } else  {

           # Collapse these two types to the "common" type
   # targetType = getMathOpType(types)

          # If any of the types are different from the targetType, we need
          # to cast.
    typeMatches = sapply(types, sameType, targetType)
    if(any(!typeMatches))
       toCast = as.list(args)[[which(!typeMatches)]]
  }

  isIntType = sameType(targetType, Int32Type) || sameType(targetType, Int64Type)
  e = lapply(args, prepareValue, targetType, isIntType, toCast, env, ir, ...)

    # XXX Have to deal with different types.
  if(isIntType)
     codes = c("+" = "Add", "-" = "Sub", "*" = "Mul", "/" = "SDiv", "%/%" = "SRem")
  else 
     codes = c("+" = "FAdd", "-" = "FSub", "*" = "FMul", "/" = "FDiv", "%/%" = "FRem")


  opName = as.character(op)
  if(opName %in% names(codes))
    op = structure(BinaryOps[[ codes[ opName ] ]], names = opName)
  else
    op = structure(NA, names = opName)

  if(any(w <- sapply(e, is, "Constant"))) {
      # if any of the operands are constants
     if(all(w)) { # constant fold
         warning("should constant fold here")
     } else {
       idx = which(w)
       k = e[[ idx ]]
       v = getValue(k)
       if( (v == 0 && names(op) == "+") ||
           (v == 0 && idx == 2L && names(op) == -1) ||
           (v == 1 && names(op) == "*") ||
            (v == 1 && names(op) == "/" && which(w) == 2))
                      return(e[!w][[1]])
    }
  }


  if(!is.na(op)) {
      ins = ir$binOp(op, e[[1]], e[[2]], id = deparseCall(origCall))
  } else {
    
     if(opName == "^") {
         # also see callHandler() in call.R
       f = declareFunction(getBuiltInRoutines(env = env)[["pow"]], "pow", env$.module)
       env$addCallInfo("pow")       
       ins = ir$createCall(f, e[[1]], e[[2]])
     } else
       stop("no math operation found corresponding to ", opName)
  }

  ins
}


deparseCall =
function(call)
{
   strsplit(format(call), "\\\n")[[1]][-1]
}    
        

prepareValue =
    #
    # The converts the argument in a binary operation call into the appropriate form for that operation.
    # The argument can be a literal, a Symbol identifying a variable/parameter, a previously compiled
    # expression
    #
function(x, targetType, isIntType, toCast, env, ir, ...)
{
     if(is(x, "Symbol"))
        x = as.name(x$name)
     else if(is(x, "Literal"))
        x = x$value
     else if(is(x, "Call"))
        return(compile(x, env, ir, ..., .targetType = targetType))
     else if(is(x, "Brace") && x$is_paren)
         #XXX What if more than on element in paren - possible?
        return(compile(x$body[[1]], env, ir, ..., .targetType = targetType))     
     else if(is(x, "ASTNode"))
        stop("need to deal with this type of ASTNode")

     if(is(x, "Value")) {
        if (!sameType(targetType, Rllvm::getType(x)))   # !is.null(toCast) && identical(x, toCast)
           createCast(env, ir, targetType, Rllvm::getType(x), x)
         else
           x
     } else if(is(x, "numeric")) {

        if(isIntType)
          createIntegerConstant(as.integer(x))
        else if(sameType(targetType, DoubleType))
          createDoubleConstant(as.numeric(x))
        else
          createFloatingPointConstant(as.numeric(x), type = FloatType)
     } else if(is.name(x)) {
        var = getVariable(x, env, ir)

            # don't load if this is an Argument, but do load if it is a local variable.
#browser()        
        if(is(var, "AllocaInst")) #XXX????  Was !is(, "Argument")
           var = ir$createLoad(var)
        
        if (!sameType(targetType, Rllvm::getType(var)))   # !is.null(toCast) && identical(x, toCast)
              # Casting to double needed
          return(createCast(env, ir, targetType, Rllvm::getType(var), var))
        else 
          return(var)
     } else
       compile(x, env, ir, ...)
}
