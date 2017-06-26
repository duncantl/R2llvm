logicOpHandler =
# This handles all logical operators, binary and unary (which is only
# !). This includes: ==, !=, >, <, >=, <= for both real and integer
# values (so far). Type coercion is needed, as LLVM has type-specific
# comparison operators like FCmp.
    #
    #  Where are &, &&, | and || handled.
    #  in createConditionCode(), etc.
    #  See tests/fib.R for an example.
    #
function(call, env, ir, ...)
{
browser()
   if(length(call$args) == 1) {
        # Unary operation
     val = compile(call$args[[1]], env, ir, ...)
     ty.val = getType(val)
     if(sameType(ty.val, Int1Type))
       not = ir$createNot(val)
     else if(isIntegerType(ty.val))  { #sameType(getType(val), Int32Type)) {
        # could just rewrite the call to call[[2]] == 0 or whatever and then pass to code below
        # but we have already compiled call[[2]].
       not = ir$createICmp( ICMP_EQ, val, ir$createIntegerConstant(0L, getContext(env$.module)), paste(deparse(call[[2]]), collapse = ""))
     } else
        stop("Not yet handling ! for non-integer types")
     
     return(not) #, "xx", ir$getInsertBlock()))
   }

    # need to handle the different ops
    # and the different types, casting
    # if necessary.
  op = as.character(call$fn$name)

  if(is.null(call$args[[2]])) { #XXX note NULL has to be on RHS for now.
       # e.g.   ptr == NULL or ptr != NULL
     ty = getTypes(call$args[[1]], env)
          # so comparing a pointer to NULL     
     if(isPointerType(ty)) {
         a = compile(call$args[[1]], env, ir)
         b = getNULLPointer(ty) # was SEXPType, but this should be ty.
         op = if(op  == "!=") ICMP_NE else ICMP_EQ
         return( ir$createICmp(op, a, b) )
     }
  }
   
  types = lapply(call$args, getTypes, env)
  targetType = getMathOpType(types, call$args)


    # Need to change order of call[2:3] if one is a literal and the other is a character a and b, 
    # could be call[[2]] e.g.  "z" == x
  if(sameType(targetType, Int8Type) && is.character(call$args[[2]]) && nchar(call$args[[2]]) == 1) { 
      tmp = Rllvm:::asciiCode(call$args[[2]])
      b = createIntegerConstant(tmp, type = targetType, bitwidth = 8L) # type is ignored
      # b = compile(tmp, env, ir)
  } else
      b = compile(call$args[[2]], env, ir)
   
  a = compile(call$args[[1]], env, ir)


  if(all(sapply(types, sameType, StringType))) {
      e = substitute(strcmp(a, b) == 0L, list(a = a, b = b))
      return(compile(e, env, ir, ...))
  }


  isIntType = sameType(targetType, Int32Type)   || sameType(targetType, Int8Type)  
   

  if(isIntType) {
     codes = c("==" = ICMP_EQ, "!=" = ICMP_NE, ">" = ICMP_SGT, "<" = ICMP_SLT, ">=" = ICMP_SGE, "<=" = ICMP_SLE)
  } else {
     codes = c("==" = FCMP_UEQ, "!=" = FCMP_UNE, ">" = FCMP_UGT, "<" = FCMP_ULT, ">=" = FCMP_UGE, "<=" = FCMP_ULE)

    # Coerce type TODO replace with more generic type coercion?
    vars = list(a, b)
    convert.i = which(sapply(types, function(x) !sameType(x, targetType)))   # sameType replaces identical
    if(length(convert.i))
       vars[[convert.i]] = createCast(env, ir, targetType, types[[convert.i]], vars[[convert.i]])
    a = vars[[1]]
    b = vars[[2]]
 }

  op = codes[ op ]
  if(is.na(op))
     stop("Unhandled logical operator")

  if(isIntType)
    ir$createICmp(op, a, b)    
  else
    ir$createFCmp(op, a, b)

}
