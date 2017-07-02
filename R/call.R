compile.call = 
  #
  # This handles calls to other functions.
  #
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL, .useHandlers = TRUE)
{
   if(is(call, "Call")) {
       funName =  as.character(call$fn$name)
       args = call$args
       astCall = TRUE
   } else {
       funName = as.character(call[[1]])
       args = as.list(call[-1])
       astCall = FALSE
   }

cat("compile.call for",funName, "\n")   
#browser()

   if(.useHandlers && funName %in% names(env$.compilerHandlers))
       return(dispatchCompilerHandlers(call, env$.compilerHandlers, env, ir, ...))

   rtype = NULL

        # Can probably remove the following first if() since that is now in dispatchCompulerHandlers.
   if(funName == "<-" || funName == "=" || funName == "<<-")
      return(env$.compilerHandlers[["<-"]](call, env, ir, ...))  #XXX should lookup the  or "=" - was `compile.<-`
   else if(funName %in% c("numeric", "integer", "character", "logical")) {
     if(length(args) == 0)
       args = list(1L) #XXX or 0 for an empty vector?


     rtype = switch(funName,
                     numeric = "REALSXPType",
                     integer = "INTSXPType",
                     character = "STRSXPType",
                     logical = "LGLSXPType")

     call = substitute(Rf_allocVector(type, len), list(type = getSEXPTypeNumByConstructorName(funName), len = args[[1]]))
     funName <- "Rf_allocVector"
     args = as.list(call[-1])
#     call$args[[2]] = Integer$new(call$args[[1]]$value)
#     call$args[[1]] = Integer$new(getSEXPTypeNumByConstructorName(funName))
#     call$fn = Symbol$new( funName <- "R_allocVector")
#XXX Register this with LLVM, i.e. llvmAddSymbol()
     
   } else if(funName == "$") {
      return(env$.compilerHandlers[["$"]](call, env, ir, ...))
   } else if(funName %in% c(".typeInfo", ".signature")) {
      return(TRUE)  # we already need to have the type information to create the Function(), so this is a No-Op
   } else if(funName == ".varDecl") {
       vars = eval(call)
       env$.localVarTypes[names(vars)] = vars
       return(TRUE)
   } else if(funName == ".R" || funName %in% names(env$.CallableRFunctions)) {
       return(callRFunction(call, env, ir, ...))
   } else if(funName == ".debug") {
         if(!env$.debug)
           return(FALSE)

         if(length(call) > 2 && (length(names(call)) == 0 || any(names(call) == "")))
             warning("extra unnamed arguments to .debug(). Is this an error in closing parentheses?")
         
         call = call[[2]]
         funName = as.character(call[[1]])
    } else if(length(env$.assertFunctions) && funName %in% env$.assertFunctions) {
          # Add support for structured errors in assertions.
         if("class" %in% names(call)) {
             classes = call[["class"]]
             if(!is.character(classes))
                classes = as.character(classes[-1])  # not evaluating these
             classes = c(classes, "error") 
             call = substitute(if(! cond) R_va_raiseStructuredError(msg, nclass),
                                 list(cond = call[[2]], nclass = length(classes),
                                      msg = sprintf("%s assertion not satisfied", paste(deparse(call[[2]]), collapse = " "))))
            call[[3]][3 + seq(along = classes)] = classes
            llvmAddSymbol(getNativeSymbolInfo("R_va_raiseStructuredError", "RLLVMCompile"))
         } else
            call = substitute(if(! cond) Rf_error(msg), list(cond = call[[2]], msg = sprintf("%s assertion not satisfied", paste(deparse(call[[2]]), collapse = " "))))
         return(compile(call, env, ir, ...))
    } else if (funName %in% c("stop", "warning")) {
        classes = character()
        if("class" %in% names(call)) {
             classes = call[["class"]]
             if(!is.character(classes))        
                   classes = as.character(classes[-1])  # not evaluating these
        } 
        classes = c(classes, if(funName == "stop") "error" else "warning")

        msg = call[[2]]
        err = substitute(R_va_raiseStructuredError(msg, nclass), list(msg = msg, nclass = length(classes)))
        err[3 + seq(along = classes)] = classes
        llvmAddSymbol(getNativeSymbolInfo("R_va_raiseStructuredError", "RLLVMCompile"))        

        return(compile(err, env, ir, ...))        
    }

   
       #XXX may not want this generally, but via an option in env or just have caller invoke compileSApply() directly.
       #  See fgets.Rdb in Rllvm/
   if(!is.null(type <- getSApplyType(call, env, funName))) {
      fun = env$.module[[ as.character(call$args[[2]]) ]]
      rt = getFunctionReturnType(fun)
      e = rewriteSApply(call, type, rt, env = env, ir = ir) # return type of routine being called.
      ans = lapply(e, compile, env, ir, ...)
      return(ans[[length(ans)]])
   }

 #XXX Can this happen now that rewrite it above to use Rf_allocaVector()?
   if(isPrimitiveConstructor(call))  
     return(compilePrimitiveConstructor(funName, call, env, ir, ...))

   # switch and other special functions.
   
   funName = mapRoutineName(funName)

   if(funName == "mkList") {
      e = substitute(Rf_allocVector(type, len), list(type = VECSXP, #getSEXPTypeNumByConstructorName(funName),
                                                     len = args[[1]]))
      obj = compile.call(e, env, ir, ...)
      return(obj)
   } else  if(funName == "list") {
      # Need to allocate the list, protect it and then evaluate the elements and insert them.
      #
      e = substitute(Rf_allocVector(type, len), list(type = getSEXPTypeNumByConstructorName(funName), len = length(args)))
      obj = compile.call(e, env, ir, ...)
#XXX Need to assign this to a variable before we can use it in the next few calls.
      
      # A rewrite of the AST will put the calls to protect and unprotect.
#TURN OFF IF .rewriteAST  is TRUE
#  e = substitute(Rf_protect(obj), list(obj = obj))
#  protect = compile.call(e, env, ir, ...)
      for(i in seq(along = args)) {
          e = substitute(SET_VECTOR_ELT(obj, pos, val), list(obj = obj, pos = i-1L, val = args[[i]]))
          compile.call(e, env, ir, ...)
      }
#TURN OFF IF .rewriteAST  is TRUE      
#  e = substitute(Rf_unprotect_ptr(obj), list(obj = obj))
#  compile.call(e, env, ir, ...)
      return(obj)
   }


   v <- getVariable(funName, env)
   ofun = NULL
   if(!is.null(v)) {

       ty = getElementType(getType(v))
       if(getTypeID(ty) == Rllvm:::PointerTyID && getTypeID(getElementType(ty)) == Rllvm:::FunctionTyID) {
           ofun = ir$createLoad(v)
           pfun = getElementType(ty)
           targetTypes = .Call("R_getFunctionTypeArgTypes", pfun, PACKAGE = "Rllvm")
       }

   } 
    # Here we utilize the polymorphic nature of intrinsics.
    # We may not want this flexibility. e.g. if we have an integer
    # and are calling log(), then we get an int32 returned. We probably
    # want to coerce the input up to a double and use the regular log() fn.
#XXX remove the intrinsics here as problems on Linux.
   if(FALSE && isIntrinsic(funName)) {
      argTypes = lapply(as.list(call[-1]), getTypes, env)
      ofun = getIntrinsic(env$.module, funName, argTypes)
   } else if(is.null(ofun)) {
      ofun = findFun(funName, env)
   
    #??? Need to get the types of parameters and coerce them to these types.
    # Can we pass this to compile and have that do the coercion as necessary

      targetTypes = getParamTypes(funName, env, TRUE)
  }


     # if we have a mismatch between the length of targetTypes and call (w/o the function name)
     # we either have ... or an error.
   if(length(targetTypes) < length(call$args)) {

       if(isVarArg(ofun)) {
             # targetTyps has a TRUE in it
          d = (length(args) -  length(targetTypes))
          targetTypes[ seq(1, d) + length(targetTypes) ] = replicate(d, NULL, simplify = FALSE)
       } else {
          msg = paste("incorrect number of parameter types for call to ", funName, ". Expected ", length(targetTypes), " had ", length(call$args), sep = "")
          err = structure(c(simpleCondition(msg), compileCall = call, paramTypes = targetTypes, func = funName), class = c("WrongNumArgs", "UserError", "CompilerError", "error", "condition"))
          stop(err)
       }
   }
#browser()
#if(funName == "REAL") browser()   
   args = mapply(function(e, ty)
                   compile(e, env, ir, ..., .targetType = ty),  # ... and fun, name,
                 args, targetTypes)
   
   env$addCallInfo(funName)

   w = sapply(args, is, "AllocaInst")
   args[w] = lapply(args[w], function(x) ir$createLoad(x))
  
   call = ir$createCall(ofun, .args = args)
   if(isTailFunction(env$.Rfun, env$.hints))
     setTailCall(call)

   if(!is.null(rtype)) 
       attr(call, "RType") = rtype

   
   # If pass an aggregate by value
   #     setArgByVal(call, 1L)
   
   
   call
}




addFun =
function(env, name, returnType, params)
{
  env$.builtInRoutines[[name]] = c(returnType, params)
}

findFun =
function(id, env)
{
  funcs = getModuleFunctions(env$.module)
  if(id %in% names(funcs))
     return(funcs[[id]])
  else if(id %in% names(env$.builtInRoutines)) 
     return(declareFunction(env$.builtInRoutines[[id]], id, env$.module))

  stop("Can't reference function ", id, " in module ") #, getName(env$.module))
}


isTailFunction =
function(fun, hints)
     inherits(fun, "TailFunction")




compilePrimitiveConstructor =
function(funName, call, env, ir, ...)
{
  if(length(call) > 1)
    warning("ignoring the second argument for call")
  
  val = switch(funName,
               character = "",
               string = "",
               integer = 0L,
               numeric = 0,
               logical = TRUE)
  
   compile(val, env, ir, ...)
}



getParamTypes =
    #
    # name is the name of the routine being called
    #
    # Have to be careful this is not called for an R function.
    # if it is, we have the type information in .CallableRFunctions
    
function(name, env, discardVarArgs = FALSE)
{
   f = env$.builtInRoutines[[ as.character(name) ]]
   if(is.null(f)) {
      f = env$.module[[ as.character(name) ]]
      ans = lapply(getParameters(f), Rllvm::getType)
   } else
      ans = f[-1]
   
   if(discardVarArgs && !is.na((i <- match("...", names(ans)))))
       ans[ - i]
   else
       ans
}



