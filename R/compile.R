## compile.R - compile R functions
# Some of this was inspired by Tierney's compiler package.

MathOps = c("+", "-", "*", "/", "%/%", "^")
LogicOps = c("<", ">", "<=", ">=", "!=", "==", "!")

getBasicType =
function(call)
{
   id =  as.character(call[[1]])
   switch(id,
            integer = Int32Type,
            string = ,
            character = StringType,
            numeric = DoubleType,
            logical = Int1Type,
            float = FloatType)
}

isPrimitiveConstructor =
function(call)
{
  if(is.call(call) && as.character(call[[1]])  %in% c("integer", "string", "character", "numeric", "logical", "float"))
    TRUE
  else
    FALSE
}

setGeneric("isSubsettingAssignment", function(call) standardGeneric("isSubsettingAssignment"))

tmp =
function(call) {  
  length(call) > 1 &&
   is.call(tmp <- call[[2]]) &&
    as.character(tmp[[1]]) %in% c("[", "[[")
}    
setMethod("isSubsettingAssignment", "call", tmp)
setMethod("isSubsettingAssignment", "=", tmp)
setMethod("isSubsettingAssignment", "<-", tmp)

setMethod("isSubsettingAssignment", "Assign", function(call) FALSE)


tmp =
function(call)          
{  
  length(call$args) > 0 &&
   is(tmp <- call$args[[1]], "Call") &&
    as.character(tmp$fn$name) %in% c("[", "[[")
}
setOldClass("Call")
setOldClass("Replacement")
setMethod("isSubsettingAssignment", "Call", tmp)
setMethod("isSubsettingAssignment", "Replacement", function(call) TRUE)



assignHandler = `compile.=` =   # `compile.<-` 
  # Second version here so I don't mess the other one up.
  #
  # XXX   This is now getting too long. Break it up and streamline.
  #
function(call, env, ir, ..., .targetType = NULL, .useHandler = TRUE)
{

#   if(.useHandler && !is.na(i <- match(as.character(call[[1]]), names(env$.compilerHandlers)))) 
#       return(env$.compilerHandlers[[i]](call, env, ir, ...))

   if(is(call, "Replacement"))
      call = asRCall(call)

   if(is(call, "Call"))
      args = call$args
   else if(is(call, "Assign"))
      args = list(call$write, call$read)
   else
      args = as.list(call[-1])  # drop the = or <-
   
   stringLiteral = FALSE
   type = NULL


   if(is(args[[2]], "Symbol")) {
       # Experimenting with mapping an SSA name to its basename
       # when we have never allocated a variable for the ssa name.
       # See tests2/  assignSubset.R and also list.R.
       #
       if(args[[2]]$basename %in% names(env$.params)) 
           args[[2]] = env$.params[[v$basename]]
   }

   
#XXX may not need to do this but maybe can compile the RHS as usual.
   if(isSubsettingAssignment(call) && is(ty <- getElementAssignmentContainerType(args[[1]], env), "STRSXPType")) 
      return(assignToSEXPElement(args[[1]], args[[2]], env, ir, type = ty))

#!!!!!XXX The Replacement object is different from the way R represents the assignment
# In R,   x[1L] = 10 is represented as  =(x[1L], 10) - so a call with 2 arguments.
# The Replacement class inherits from Cal and the function being called is [<- and
# There are 3 arguments to this call x, 1L, and 10   
# In an expression such as x[1, 2] = foo(3, 4)
# we get 4 arguments x, 1, 2 and foo(3, 4).



       # look at the RHS - is this the RHS?
   rhs = args[[length(args)]]
   if(isLiteral(rhs)) {  #!! these are the args, not the call - so first element is not = or <-, but the LHS.
         # Do we need to eval() this. If it is really literal, no.
      tmp = val = if(is(rhs, "Literal")) rhs$value else eval(rhs)
      ctx = getContext(env$.module)

       
      lhs.type = getDataType(args[[1]], env)
        # What is this doing?
        # So not a character, and we don't know the lhs type or we know it isn't
      if( !is.character(tmp) && (is.null(lhs.type) || !sameType(DoubleType, lhs.type))  && env$.integerLiterals  && val == floor(val) )
          tmp = val = as.integer(val)

      if(!is.null(lhs.type))
          type = lhs.type
      else
           type = getDataType(I(val), env)

#XXX  What if this is an expression??
      val = makeConstant(ir, val, type, ctx)
      type = getDataType(val, env)
      if(is.character(tmp)) 
         stringLiteral = TRUE

   } else if(FALSE && isPrimitiveConstructor(args[[2]])) {  # what does skipping this break? any code where we have an integer() or whatever and use it as int *
       # so this is probably just defining a variable.
       # Use the type of the RHS to create the variable.
       # Perhaps just change compile.call to handle these functions
       # specially  and return a val.
       #
       #  Need to know if we need to create a SEXP or just the corresponding native type
       # i.e. an INTSXP or an int [n]

       type = getBasicType(args[[2]])
       val = NULL
    } else
      val = compile(args[[2]], env, ir)


   if(is(args[[1]], "Symbol"))
       args[[1]] = as.name(args[[1]]$name)
   
   if(is.name(args[[1]])) {
         var = as.character(args[[1]])

#XXXX Temp to check something
         
      # We don't search parameters for the var name, since we don't
      # want to try to assign over a parameter name.
      #XXX  I think we do want to mimic that behaviour but understand which local variable that corresponds to.
         ref <- getVariable(var, env, ir, load = FALSE, search.params = FALSE)
         if(is.null(ref)) {

                 # No existing variable; detect type and create one.
          if(is.null(type)) 
            type = env$.localVarTypes[[var]]

          if(is.null(type))
             type = env$.types[[var]]
          
          if(is.null(type)) 
             type = getDataType(var, env)

                 # didn't get a type from the variable, so look at the RHS.
          if(is.null(type)) 
             type = getDataType(val, env, args[[2]])

        
          if (is.null(type)) {
                   # Variable not found in env or global environments; get type via Rllvm
              if (is(val, "StoreInst")) {
                  # This is from the val = compile(); probably from a
                  # statement like: y <- 4L; x <- y. When args[[2]] is
                  # compiled above, getVariable returns an object of class
                  # StoreInst. We ignore the current val, and instead query
                  # the type from the variable.
                  type = getDataType(args[[2]], env)
              }

              if(is(val, "Value"))
                  type = getDataType(val, env)
          }
          
          #XXXX Merge with compile.character
          if(stringLiteral) {  # isStringType(type)) 
              gvar = createGlobalVariable(sprintf(".%s", var), env$.module, type, val, TRUE, PrivateLinkage)
              val = getGetElementPtr(gvar, ctx = ctx)
              type = StringType
          }

          ref <- createFunctionVariable(type, var, env, ir)
          env$newAlloc(var, ref)
   #XXX      assign(var, ref, envir = env)
          
           ## Todo fix type ???
          if(!(var %in% names(env$.types)))
            env$.types[[var]] = type
       }
   } else {

      expr = args[[1]]

         # XXX  have to be a lot more general here, but okay to be simple for now (Apr 26 2013).
      if(is(ty <- getElementAssignmentContainerType(expr, env), "SEXPType")
          && is.null(expr <- assignToSEXPElement(expr, val, env, ir, ty)))
            return(val)

      #XXXX What does removing load = FALSE affect. Find examples of where this breaks matters.
      # fuseLoops?
        ref = compile(expr, env, ir, ..., load = FALSE)
  }

   if(!is.null(val)) {

      if(all(sapply(args, is.name))) {

# Temporary hack for dealing with ._return_1 = .return_1 in rw2d.R
          tmp = env$.types[ sapply(args, as.character) ]
          if(!sameType(tmp[[1]], tmp[[2]]))
             val = createCast(env, ir, getElementType(type[[1]]), tmp[[2]], val)
          else if(is(val, "AllocaInst"))
              
             val = ir$createLoad(val)
      } else if(!sameType(getType(val), getElementType(getType(ref)))) {
#XXX
#cat("fix this cast\n")
#         val = Rllvm::createCast(ir, "SIToFP", val, getElementType(getType(ref)))
          to = getElementType(getType(ref))
          from = getType(val)
          
	  val = createCast(env, ir, to, from, val)
      }


      store = ir$createStore(val, ref)
      if(!is.null(tmp <- attr(val, "zeroBasedCounting"))) {
         attr(ref, "zeroBasedCounting") = tmp
         if(is.name(call[[2]]))
           env$.zeroBased[as.character(args[[1]])] = TRUE
      }
   }

# Probably don't want to set this anymore, or certainly not for x[1] but only when is.name(args[[1]])
   setVarType(env, args[[1]], val)


   val  # return value - I (Vince) changed this to val from ans (the createStore() return).
        # There seems to be very little we can do with the object of class
        # StoreInst. Note: this seems to be the way it's done here
        # too: http://llvm.org/docs/tutorial/LangImpl7.html
}


setVarType =
    #
    # This adds the type to 
    #
function(env, lhs, rhs, meta = FALSE)
{
   if(is.name(lhs)) {
       lhs = as.character(lhs)
       ty = getType(rhs)
       if(!is.null(tmp <- attr(rhs, "RType")))
           ty = get(tmp, globalenv(), inherits = TRUE)
       env$.localVarTypes[[ lhs ]] = ty

       if(meta)
           setMetadata(env$.module, lhs, class(ty))
   }
}

getElementAssignmentContainerType =
  #
  #  called from just above for x[.....]
  #
function(call, env)
{
   var = if(is.name(call))
           call
         else {
          if(is(call, "Call"))
              call$args[[1]] # [[2]]
          else
              call[[2]]
         }

   getDataType(var, env)
}

createFunctionVariable =
  #
  # create a local variable, but put it in the entry block of the function
  # rather than in the current block.  The idea is that we don't
  # want to allocate variables in a loop and so end up repeating that instruction.
  #
function(type, id, env, ir)
{
  cur = getInsertBlock(ir)
  setInsertPoint(ir, env$.entryBlock)
  on.exit(setInsertPoint(ir, cur))
  var = createLocalVariable(ir, type, id, TRUE)  # to insert before terminator.
  env$newAlloc(id, var)
  var
}


compile <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL, .useHandler = TRUE)
{
   if(is(e, "RC++Reference")) # for already compiled objects, i.e. Value.
      return(e)

   if(.useHandler && is.call(e)) {
      tmp = dispatchCompilerHandlers(e, env$.compilerHandlers, env, ir, ...)
      if(!is.null(tmp))
        return(tmp)
   }
   
    # This doesn't always seem to dispatch on <-, i.e. in the code generated by rewriteSApply(). That is just a call.
  if(is.call(e) && as.character(e[[1]]) %in% c("<-", "=", "<<-"))
    `compile.=`(e, env, ir, ...)
  else
     UseMethod("compile")
}

compile.character =
  # See varargs.R which is a call to printf() with a constant format
function(e, env, ir, ..., .targetType = NULL)
{
   ctxt = getContext(env$.module)
   ty = arrayType(Int8Type, nchar(e))
   strVar = createGlobalVariable(".tmpString", env$.module, val = e, constant = TRUE, linkage = PrivateLinkage)
   return(getGetElementPtr(strVar, ctx = ctxt))
   
   ptrVar = createFunctionVariable(StringType, ".tmpStringPtr", env, ir) 
   setInitializer(ptrVar, strVar)
   createLoad(ir, ptrVar)
}


`compile.{` = compileExpressions =
  #
  # This compiles a group of expressions.
  # It handles moving from block to block with a block for
  # each expression. <Is this still true? or is it more sophisticated about blocks now?>
function(exprs, env, ir, fun = env$.fun, name = getName(fun), .targetType = NULL, ..., afterBlock = NULL, nextBlock = NULL)
{
  #insertReturn(exprs)
   given.afterBlock = !missing(afterBlock)
   
  if(as.character(exprs[[1]]) != "{")
      compile(exprs, env, ir, fun = fun, name = name)
  else {
     oldVals = env$.remainingExpressions
     on.exit(env$.remainingExpressions <- oldVals)
      
    exprs = exprs[-1]

    idx = seq_along(exprs)

    for (i in idx) {
        cur = ir$getInsertBlock()
        if(length(getTerminator(cur))) 
            break

        env$.remainingExpressions = exprs[ - (1:i) ]

        pop = FALSE
        if(is.call(exprs[[i]]) && (is(exprs[[i]], "if") || is(exprs[[i]], "for") || is(exprs[[i]], "while")) ) {

            if(i < length(idx))  # length(afterBlock) == 0 &&
                afterBlock = if(length(afterBlock)) afterBlock else Block(env$.fun, sprintf("after.%s", deparse(exprs[[i]])))
            else { 
                afterBlock = nextBlock
               if(is(exprs[[i]], "if")) {
                    # THIS SEEMS ugly. It handles the case of a while() { } where the last expression in the {}
                    #  is an if() expr with no else.
                    #  We need to return to the while() condition, not jump to nextBlock. 
                   if(length(env$.loopStack) && env$.loopStack[1] == "while") {
                      tmp = list(...)$nextIterBlock
                      if(!is.null(tmp)) 
                         afterBlock = tmp
                      else
                         stop("probably something wrong!!!")
                   }
               }
            }

            pop = TRUE                
            #pushNextBlock(env, afterBlock)
        }
        
        compile(exprs[[i]], env, ir, fun = fun, name = name, nextBlock = afterBlock, ...)

        if(pop) {
             # Do we setInsertBlock() for this next block?
            #popNextBlock(env)  # popping the wrong thing!
            b = afterBlock
            afterBlock = NULL
            if(!is.null(b))
                setInsertBlock(ir, b)
        }
#        # One approach to handling the lack of an explicit return is to
#        # create the return instruction ourselves, or to add a return
#        # around the call before we compile. The advantage of the latter
#        # is that any code generation that we write to ensure the correct
#        # return type on the expressions e.g. return(x + 1) will do the correct
#        # thing on the x + 1 part, not later converting the value.
#      if(i == idx[length(idx)] && !is.call(exprs[[i]]) || exprs[[i]][[1]] != as.name('return')) { # last one
#        ir$createReturn(val)
#      } else
#        val
      }
  }
}


compile.name <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL)  
{
   getVariable(e, env, ir, searchR = TRUE, load = TRUE, ...)
}

compile.integer <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL)  
{
   if(length(e) == 1)
      createIntegerConstant(e, type = .targetType)
   else
     stop("not compiling integer vector (multiple values) for now")
}

compile.logical <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL)  
{
 # compile(as(e, "integer"), env, ir, ..., fun = fun, name = name)
  createLogicalConstant(e)
}


compile.numeric <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL)  
{
  if(length(e) == 1) {
     if(length(.targetType))
       createConstant(val = e, type = .targetType)    
     else
       createDoubleConstant(e)
  }
  else
     stop("not compiling numeric vector for now")
}


compile.Value <-
  # This is just an LLVM value
function(e, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL)  
  e

if(FALSE) {
compile.ASTNode =
function(e, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL)
    construct_ir(e, env, ir, env$.types)
}

compile.default <-
function(e, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL)
{
    if(is(e, "("))
       return(compile(e[[2]], env, ir, .targetType = .targetType))
    
    if(is(e, "Value") || is(e, "Instruction"))
      return(e)
    

    if (is.call(e)) {
      dispatchCompilerHandlers(e, env$.compilerHandlers, env, ir, ...)
    } else if (is.symbol(e)) {
      var <- as.character(e)
      return(var) ## TODO: lookup here, or in OP function?
    } else if(is.character(e)) 
       return(compile.character(e, env, ir, ...))
    else
      stop("can't compile objects of class ", class(e))
}

dispatchCompilerHandlers =
    #XXX Dispatch across lists.
function(e, handlers, env, ir, ...)
{
           # Recursively compile arguments
    call.op <- findCall(e[[1]], env$.compilerHandlers)
    if(is.null(call.op))
        return(NULL)
      
    if(is.list(call.op)) {
        for(f in call.op) {
            tmp = f(e, env, ir, ...)
            if(!is.null(tmp))
                return(tmp)
        }
    } else {
         if (typeof(call.op) != "closure" && is.na(call.op)) 
           call.op = findCall("call", env$.compilerHandlers)

         if(!is.list(call.op) && !is.function(call.op) && is.na(call.op))
            return(NULL)

          # XXX Dispatch across list here if we get back a list.
         call.op(e, env, ir, ...)
     }
}    


findGlobals=
function(fun, merge = FALSE, ignoreDefaultArgs = TRUE)
{
  ans = codetools::findGlobals(fun, merge)
  if(!merge && ignoreDefaultArgs) {
     formals(fun) = lapply(formals(fun), function(x) NULL)
     ans$functions = codetools::findGlobals(fun, FALSE)$functions
  }
  ans
}


addArgSEXPTypeMetadata =
function(className, id, module)
{
  name = sprintf("%s.SEXPType", id)
  setMetadata(module, name, list("SEXPType", className))
}


addSEXPTypeMetadata =
function(module, argTypes)
{
  k = sapply(argTypes, class)
  i = (k != "SEXPType")
  if(any(i)) 
      mapply(addArgSEXPTypeMetadata,  k[i], names(argTypes)[i], MoreArgs = list(module = module))
  
  any(i)
}




compileFunction <-
function(fun,
         cfg = to_cfg(fun),
         types = infer_types(cfg),
         returnType = return_type(types), 
         module = Module(name),
         name = NULL,
         compiler = makeCompileEnv(),
         NAs = FALSE,
         asFunction = FALSE, asList = FALSE,
         optimize = TRUE, ...,
         .functionInfo = list(...),
         .routineInfo = list(),
         .compilerHandlers = getCompilerHandlers(),
         .globals = getGlobals(if(isClosure) fun else to_r(fun),
                               names(.CallableRFunctions),
                               .ignoreDefaultArgs, .assert = .assert, .debug = .debug), #  would like to avoid processing default arguments.
                                 # findGlobals(fun, merge = FALSE, .ignoreDefaultArgs), 
         .insertReturn = !identical(returnType, VoidType),
         .builtInRoutines = getBuiltInRoutines(),
         .constants = getConstants(),
         .vectorize = character(), .execEngine = NULL,
         structInfo = list(),
         .ignoreDefaultArgs = TRUE,
         .useFloat = FALSE,
         .zeroBased = TRUE,
         .localVarTypes = list(),
         .fixIfAssign = TRUE,
         .CallableRFunctions = list(), 
         .RGlobalVariables = character(),
         .debug = TRUE, .assert = TRUE,
         .addSymbolMetaData = TRUE,
         .readOnly = constInputs(if(is(fun, "ASTNode")) eval(to_r(fun)) else fun),
         .integerLiterals = TRUE,
         .loadExternalRoutines = TRUE,
         .rewriteAST = missing(cfg)
         )  # .duplicateParams = TRUE
{
   if(missing(name)) 
      name = deparse(substitute(fun))

   if(is.logical(.assert))
      .assert = if(.assert) ".assert" else character()

#this probably goes   
   if(!missing(types) && !is.list(types))
      types = structure(list(types), names = names(formals(fun))[1])

   if(is.logical(.execEngine) && .execEngine)
      .execEngine = ExecutionEngine(module)
      
   if(!missing(fun) && .fixIfAssign)
     fun = fixIfAssign(fun)

  isClosure <- typeof(fun) == "closure"

   
  if (isClosure || is(fun, "Function")) {

      # In the case tht rewriteAST changes the computational nature of the code,
      # we compute the types from the original code. We can then augment the resulting
      # types computed from the rewritten AST and CFG with any from the types computed here that are missing from the
      # rewrites.
   pre.types = infer_types(fun, error = FALSE)

   if(.rewriteAST && missing(cfg)) {
       # What if person specified the cfg?
       if(is(fun, "ASTNode"))
           ast = fun
       else
           ast = to_ast(fun)
       rewriteAST(ast)
       cfg = to_cfg(ast)
   }

       # Should this be before .rewriteAST?
     if(missing(cfg) && .insertReturn && isClosure)
       fun = insertReturn(fun) # do we need env??
                               #  Doing this here because we need to insert the returns before the CFG and types.

       #if there is no type information but the author put the type information on the function itself, use that.
     .typeInfo = attr(fun, "llvmTypes")
     if( (missing(returnType) || missing(types)) && !is.null(.typeInfo)) {
       if(missing(types))
          types = .typeInfo$parms
       if(missing(returnType))
          returnType = .typeInfo$returnType
     }

     # for checking against types; TODO
    args <- if(isClosure) formals(fun) else fun$params

     
    if(length(types) == 0 && length(args) > 0) {
        types = getTypeInfo(fun)
        returnType = types[[1]]
        types =  types[[2]]
    }


   # Merge pre.types with types
     # We probably just want the parameters from pre.types.
   ptypes = pre.types[names(args)]
   a = intersect(names(pre.types), names(args))
   m = !(a %in% names(types))
   if(any(m))
       types[ a[m] ] = pre.types[a[m]]

   
    if(length(args)  > length(types)) 
       stop("need to specify the types for all of the arguments for the ", name, " function")


   types = lapply(types, translate_type)
   if(is(returnType, "typesys::list|Type"))
       returnType = translate_type(returnType)

#XX Revisit when the switch to the new types is working
if(FALSE) {
      # See if we have some SEXP types for which we may need to know the length.
      # This might go as we can call Rf_length().  nrow()
    rVecTypes = sapply(types, isRVectorType)
    if(any(rVecTypes)) {
       lengthVars = sprintf("_%s_length", names(types)[rVecTypes])
       types[lengthVars] = replicate(length(lengthVars), Int32Type)
    } else
       lengthVars = character()
}
     
    # Grab types, including return. Set up Function, block, and params.
    isDimensionedType = sapply(types, is, "DimensionedType")

       # The dimTypes need to have names or we need to be able to map them back to the particular arguments. They do!
    if(any(isDimensionedType)) {
       dimTypes = types[isDimensionedType]
       types[isDimensionedType] = replicate(sum(isDimensionedType), SEXPType) #XXXX Not necessarily a SEXPType anymore.
    } else
       dimTypes = list()



       # Create the LLVM Function.

                # not all the types, just the parameter types and translate them from .
     #??? Probably best to convert all the types to Rllvm types now and also to change
     # the names of the parameters to SSA form now. Could change the type and CFG to use the parameter names for
     # first reference to parameter rather than appending _1
    argTypes <- getFunParamTypes(types, names(args))
    llvm.fun <- Function(name, returnType, argTypes, module)


    if(any( i <- sapply(argTypes, is, "SEXPType")))
       addSEXPTypeMetadata(module, argTypes[i])

      # if we picked up any .R() expressions in the function, add the resulting types
      # to the .CallableRFunctions.
    if(length(.globals$skippedExpressions) &&
                (i <- names(.globals$skippedExpressions) == ".R")) {
        
        z = structure(lapply(.globals$skippedExpressions[i],
                               function(x)
                                  eval(x[[3]])),
                       names = sapply(.globals$skippedExpressions[i], function(x) as.character(x[[2]][[1]]))) # have to deal with obj$f() in x[[2]][1]]
        .CallableRFunctions = c(.CallableRFunctions, z)
    }
        
    
    if(any(.globals$functions %in% names(.builtInRoutines))) {
       i = match(.globals$functions, names(.builtInRoutines), 0)
       .routineInfo = .builtInRoutines[ i ]
       .globals$functions = .globals$functions[i == 0]
    }

    if(length(.CallableRFunctions)) {
       i = match(names(.CallableRFunctions), .globals$functions, 0)
       .globals$functions = .globals$functions[ i == 0]
    }
     

    if(name %in% .globals$functions && !(name %in% names(.functionInfo)))
       .functionInfo[[name]] = list(returnType = returnType, params = types)



    cfg.blocks = cfg$blocks[ rev(rstatic::postorder(cfg)) ]
#    names(cfg.blocks)[1] = "entry"
    blocks = lapply(names(cfg.blocks), function(i) Block(llvm.fun, i))
    names(blocks) = names(cfg.blocks)


    params <- getParameters(llvm.fun)  # TODO need to load these into nenv
 # probably don't need this later but we do set it in the nenv for now.
    block = blocks[[1]] 
    ir <- IRBuilder(block)



#XXX temporary to see if we should declare and load these individually when we encounter them
# Really need the user to specify the DLL not just the name in case of ambiguities, so often easier to do this separately.
    if(.loadExternalRoutines && length(.routineInfo))
        processExternalRoutines(module, .funcs = .routineInfo, .addMetaData = .addSymbolMetaData)

    if(length(.globals$functions)) 
       compileCalledFuncs(.globals, module, .functionInfo, optimize = FALSE)

    if(length(.globals$variables)) {

       .globals$variables = setdiff(.globals$variables, c(ExcludeGlobalVariables, .RGlobalVariables))


       i = .globals$variables %in% names(module)
       if(any(i)) {
              #XXX should check that they are actual variables and not functions.
          .globals$variables = .globals$variables[!i]
       }

#XXX     nenv & ir are not yet defined. What do we want here?
       compileGlobalVariables(.globals$variables, module, nenv, ir)
    }
    

    compiler = compiler(.compilerHandlers, NAs, .builtInRoutines, .functionInfo, structInfo, .zeroBased,
                        .integerLiterals, .useFloat, .debug, .assert, .addSymbolMetaData, .CallableRFunctions,
                        compiler = compiler)
    nenv = compiler
    nenv$.fun = llvm.fun
    nenv$.params = params
    nenv$.types = types
    nenv$.returnType = returnType
    nenv$.entryBlock = block
    nenv$.funName = name  # name of the routine being compiled.     

    nenv$.ExecEngine = .execEngine
    nenv$.module = module

    nenv$.localVarTypes = .localVarTypes
    nenv$.Constants = .constants
    nenv$.dimensionedTypes = dimTypes     

    nenv$blocks = blocks
    nenv$cfg.blocks = cfg.blocks
    nenv$.cfg = cfg


           # Doing gymnastics that should be done in rstatic. XXX Remove later
    phiVarNames = unlist(findPhiAssignVarNames(cfg))
    nenv$.phiVarInstructions = structure(vector("list", length(phiVarNames)), names = phiVarNames)



   if(isClosure)  fbody <- body(fun)
     
if(FALSE) {
    # Will insertReturn fix this?
   last = fbody[[length(fbody)]]
   if(sameType(VoidType, returnType) && is.call(last) && as.character(last[[1]]) == "if" && length(last) == 3)
      fbody[[ length(fbody) + 1L ]] = quote(return( ))
}

     
    nenv$.Rfun = fun

    if(length(.readOnly)) {
       k = .readOnly  
#       mayMutate = setdiff(names(formals(fun)), k)
       if(length(k)) {
          idx =  match(k, names(argTypes))
           if(any(is.na(idx)))
               stop("mismatch in parameter names and types")
          mapply(function(type, arg) {
                      if(isPointerType(type))
                         setParamAttributes(arg, LLVMAttributes["ReadOnly"]) # 28L, TRUE) 
                   }, argTypes[k], getFunctionArgs(llvm.fun)[idx])

       }
   }

 ### here we are going to work on the blocks in the call graph and use a different approach.
    lapply(cfg.blocks, compileCFGBlock, types, nenv, ir, llvm.fun, blocks)


     # Fix the phiForwardRefs.  May need to find the nodes
     # in additional places. Make separate function.
   w = ( names(nenv$.phiForwardRefs) %in% names(params))
   if(any(w)) 
       nenv$.phiVarInstructions[ names(nenv$.phiForwardRefs)[w] ] = params[names(nenv$.phiForwardRefs)[w]]

   actualNodes = nenv$.phiVarInstructions[names(nenv$.phiForwardRefs)]
   if(any(sapply(actualNodes, is.null)))
       stop("Problem with phi node forward references")
   mapply(replaceAllUsesWith,
           nenv$.phiForwardRefs,
           actualNodes)
     
#GONE    compileExpressions(fbody, nenv, ir, llvm.fun, name)

       # the second condition occurs when we have an if() with no else as the last expression
       # in the function. We add a return() so the compiler handlers work, and then we don't
       # add the return here.
       # Could also check for a terminator with:  getTerminator(getInsertBlock(ir))
if(FALSE) {
# Shouldn't need with CFG    
    if(identical(returnType, VoidType) && !identical(fbody[[length(fbody)]], quote(return())))
       ir$createReturn()
}


     if(length(nenv$.SetCallFuns)) {
           # This is for the callbacks to R. We have to get the expressions for the callback to the module.
           # We have a way to set them, another way to create them (although this doesn't handle complex arguments)
           # We might eliminate lots of these and just go with  serializing the expression and deserializing
           # when it is needed.
         lapply(nenv$.SetCallFuns,
                function(x)
                  compileSetCall(x$var, x$name, module))

         lapply(nenv$.SetCallFuns,
                function(x)         
                  compileCreateCallRoutine(nenv, ir, x$call, sprintf("create_%s", x$var), x$var))

         if(!is.null(nenv$.ExecEngine))   # don't use .ee as this field in the compiler(env) may have been set as a side effect of compile()
             lapply(nenv$.SetCallFuns,
                function(x)  {
                   .llvm( module[[x$name]],  x$call, .ee = nenv$.ExecEngine) 
                })

          lapply(nenv$.SetCallFuns,
                 function(x)
                   createDeserializeCall(nenv, ir, x$call, x$deserializeCallFun))

     }

    ## This may ungracefully cause R to exit, but it's still
    ## preferably to the crash Optimize() creates on an unverified module
    if(optimize && verifyModule(module))
       Optimize(module, execEngine = .execEngine)

     
    if(asFunction) 
       makeFunction(fun, llvm.fun, .vectorize = .vectorize, .execEngine = .execEngine, .lengthVars = lengthVars)
    else if (asList)
       list(mod = module, fun = llvm.fun, env = nenv)
    else
       llvm.fun
  } else if(!isIntrinsic(name))
     stop("compileFunction can currently only handle closures. Failing on ", name)
}


compilerStartFunction =
function(env, ir, name, retType, paramTypes = list())
{
  if(name %in% names(env$.module))
      f = env$.module[[name]]
  else
      f = Function(name, retType, paramTypes, module = env$.module)
  
  b = Block(f, "createCallEntry")
  ir$setInsertBlock(b)
  env$.localVarTypes = list()
  env$.returnType = if(missing(retType)) getReturnType(f)  else retType  # want the user to specify this to be able to distinguish different classes of SEXP types.
  env$.fun = f
  
  env$.entryBlock = b  # vital to set this so that the local variables go into this block.
                       # otherwise go into the entry block of the original function being compiled
                       # Need to generalize, e.g. add a method to the compiler to create a new Function

  TRUE

}


Rf_routines = c("length")
RewrittenRoutineNames = c("numeric", "integer", "logical", "character", "list", "double")

mapRoutineName =
function(name)
{
  w = name %in% Rf_routines
  name[w] = sprintf("Rf_%s", name[w])
  name
}


isRVectorType =
  # This is transient and intended to identify if the
  # type corresponds to an R vector, rather than an R scalar.
  # For now this is an arrayType(, 0), i.e. with zero elements
function(type)
{
  is(type, "ArrayType") && getNumElements(type) == 0
}


processExternalRoutines =
function(mod, ..., .funcs = list(...), .lookup = TRUE, .addMetaData = TRUE)
{
  names(.funcs) = mapRoutineName(names(.funcs))

  w = !duplicated(names(.funcs))
  .funcs = .funcs[w]

  .funcs = .funcs[setdiff(names(.funcs) , RewrittenRoutineNames)]
  
  ans = mapply(declareFunction, .funcs, names(.funcs), MoreArgs = list(mod))

  if(.lookup) {
    syms = lapply(names(.funcs),
                    function(x) {
                       info = tryCatch(getNativeSymbolInfo(x),
                                        error = function(e)
                                           as(x, "NativeSymbol"))
                       
                       if(.addMetaData && is(info, "NativeSymbolInfo")) {
                           pkg = info$package
                             # do we need the path? yes if it is not part of a package.
                             # Some of these will be "wrong", i.e. too specific
                             # e.g. finding printf in RLLVMCompile since libc is linked to RLLVMCompile.so.
                             # But that is R's problem, i.e. should be fixed there or we should specify
                             # where it is in the registration information in this package for known
                             # external routines
                           setMetadata(mod, sprintf("symbolInfo.%s", info$name),
                                            list("package", pkg[["name"]], "path", pkg[["path"]]))
                       }
                       if(is(info, "NativeSymbolInfo"))
                          info$address
                       else
                          info
                    })
    llvmAddSymbol(.syms = structure( syms, names = names(.funcs)))
  }
  ans
}


getSymbolInfoMetadata =
function(module, id = character())
{
  if(length(id) == 0) {
     # get all the metadata
     # get the names of all metadata, find those named symbolInfo\\..* and then call this function in an lapply()      
     all = getMetadata(module)
     i = grepl("^symbolInfo\\.", names(all))
     return(lapply(all[i], function(node) getSymbolInfoMetadata(module, node)))
  }

  if(length(id) > 1) {
      ans = lapply(id, function(x) getSymbolInfoMetadata(module, x))
      if(is.character(id))
          names(ans) = i
      return(ans)
  }


  md = if(is.character(id))
          md = getMetadata(module, sprintf("symbolInfo.%s", id))
       else
          id
  
  if(is.null(md))
      stop("no symbolInfo metadata for ", id)

  a = md[[1]]
  vals = names(a[])
  i = seq(1, length(vals)-1, by = 2)
  structure(vals[i+1], names = vals[i])
}


getConstants =
function(..., .defaults = ConstantInfo)
{
  vals = list(...)
  .defaults[names(vals)] = vals
  .defaults
}

getBuiltInRoutines =
  #
  # See FunctionTypeInfo also 
  #
function(..., env = NULL, useFloat = FALSE)
{

  if(!is.null(env) && exists(".builtInRoutines", env))
    return(get(".builtInRoutines", env))

  
  SEXPType = getSEXPType()

  
  basic = if(useFloat)
             list(exp = list(FloatType, FloatType),
                  log = list(FloatType, FloatType),       
                  pow = list(FloatType, FloatType, FloatType),
                  sqrt = list(FloatType, FloatType))    
          else
             list(exp = list(DoubleType, DoubleType),
                  log = list(DoubleType, DoubleType),       
                  pow = list(DoubleType, DoubleType, DoubleType),
                  sqrt = list(DoubleType, DoubleType))      


 ans =  list(

       Rf_runif = list(DoubleType, DoubleType, DoubleType),
     
       length = list(Int32Type, getSEXPType("REAL")),
       Rf_length = list(Int32Type, getSEXPType("REAL")),        # same as length. Should rewrite name length to Rf_length.
       INTEGER = list(Int32PtrType, getSEXPType("INT")),
       REAL = list(DoublePtrType, getSEXPType("REAL")),
       Rf_allocVector = list(SEXPType, Int32Type, Int32Type), #XXXX  64 or 32 type depends on platform.
       Rf_protect = list(VoidType, SEXPType),
       Rf_unprotect = list(VoidType, Int32Type),
       Rf_unprotect_ptr = list(VoidType, SEXPType),
       R_PreserveObject = list(VoidType, SEXPType),
       R_ReleaseObject = list(VoidType, SEXPType),     
       Rf_mkChar = list(getSEXPType("CHAR"), StringType),
       Rf_PrintValue = list(VoidType, SEXPType),
       STRING_ELT = list(getSEXPType("CHAR"), getSEXPType("STR"), Int32Type), # long vectors?
       SET_STRING_ELT = list(SEXPType, getSEXPType("STR"), Int32Type, getSEXPType("CHAR")), # XXX may need different type for the index for long vector support.       
       SET_VECTOR_ELT = list(SEXPType, getSEXPType("VEC"), Int32Type, SEXPType), # XXX may need different type for the index for long vector support.
#     R_SET_VECTOR_ELT = list(SEXPType, getSEXPType("VEC"), Int32Type, SEXPType), # XXX may need different type for the index for long vector support.     
       VECTOR_ELT = list(SEXPType, getSEXPType("VEC"), Int32Type),
#     R_VECTOR_ELT = list(SEXPType, getSEXPType("VEC"), Int32Type),
       SETCAR = list(SEXPType, SEXPType, SEXPType),
       SETCDR = list(SEXPType, SEXPType, SEXPType),
       SET_TAG = list(VoidType, SEXPType, SEXPType),
       Rf_install = list(SEXPType, StringType),
       CDR = list(SEXPType, SEXPType),
     
     
       Rf_nrows = list(Int32Type, SEXPType),
       Rf_ncols = list(Int32Type, SEXPType),
     
       numeric = list(REALSXPType, Int32Type),
       integer = list(INTSXPType, Int32Type),
       logical = list(LGLSXPType, Int32Type),
       character = list(LGLSXPType, Int32Type),

       Rf_ScalarInteger = list(SEXPType, Int32Type),
       Rf_ScalarReal = list(SEXPType, DoubleType),
       Rf_ScalarLogical = list(SEXPType, Int32Type),
       Rf_mkString = list(SEXPType, StringType),

       Rprintf = list(VoidType, StringType, "..." = TRUE),
       printf = list(Int32Type, StringType, "..." = TRUE),

       Rf_eval = list(SEXPType, SEXPType, SEXPType),
       Rf_asInteger = list(Int32Type, SEXPType),
     
#XXX the following are not correct and need some thinking.       
       nrow = list(Int32Type, c("matrix", "data.frame")),
       ncol = list(Int32Type, c("matrix", "data.frame")),
       dim = list(quote(matrix(Int32Type, 2)), c("matrix", "data.frame")),

       strdup = list(StringType, StringType),
       R_CHAR = list(StringType, SEXPType),

       R_loadRObjectFromString = list(SEXPType, StringType),

       Rf_error = list(VoidType, StringType, "..." = TRUE),
       R_raiseStructuredError = list(VoidType, StringType, pointerType(StringType), Int32Type),
       R_va_raiseStructuredError = list(VoidType, StringType, Int32Type, "..." = TRUE) ,

       memcpy = list(pointerType(VoidType), pointerType(VoidType), pointerType(VoidType), Int32Type),

       strcmp = list(Int32Type, StringType, StringType),
       puts = list(Int32Type, StringType),
       printf = list(Int32Type, StringType, "..." = TRUE)
     )

  ans[names(basic)] = basic

  others = list(...)
  ans[names(others)] = others
  
  ans
}

# Should this just be names of CompilerHandlers? No, need more than those.
# Although we could add these items to CompilerHandlers and have them map
# to the existing handlers, e.g., sapply = ...
# But this is not a good idea. printf is just a regular call.
ExcludeCompileFuncs = c("{", "sqrt", "return", MathOps,
                        LogicOps, "||", "&&", # add more here &, |
                        ":", "=", "<-", "<<-", "[<-", '[', "[[", "for", "if", "while",
                        "repeat", "(", "!", "^", "$", "$<-",
                        "sapply", "lapply",
                        "printf",
                        "break", "next",
                        ".R", ".typeInfo", ".signature", ".varDecl", ".pragma",
                        ".assert", ".debug",
                        "stop", "warning",
                        "logical", "integer", "numeric", "list",
                        "mkList"
                       )  # for now


compileCalledFuncs =
  #
  #  The .functionInfo
  #
function(globalInfo, mod, .functionInfo = list(), ...)
{
  funs = setdiff(globalInfo$functions, ExcludeCompileFuncs)

     # Skip the ones we already have in the module.
     # Possibly have different types!
  funs = funs[!(funs %in% names(getModuleFunctions(mod))) ]
  funs = funs[!(sapply(funs, isIntrinsic))]  
  
  funs = structure(lapply(funs, get), names = funs)


  lapply(names(funs),
           function(id) {
             if(id %in% names(.functionInfo)) {
               types = .functionInfo[[id]]
               compileFunction(funs[[id]],
                               types$returnType,
                               types = types$params,
                               module = mod, name = id,
                               ...
                               )
             } else
               compileFunction(funs[[id]], module = mod, name = id)
           })
}


makeFunction =
function(fun, compiledFun, .vectorize = character(),  .execEngine = NULL, .lengthVars = character())
{
  e = new.env()
  e$.fun = compiledFun
  e$.irCode = showModule(compiledFun, TRUE)
  
  if(is.null(.execEngine))  
     .execEngine = ExecutionEngine(as(compiledFun, "Module"))  # evaluate this now or quote it.
  
  args = c(as.name('.fun'), lapply(names(formals(fun)), as.name), as.name("..."))
  k = call('.llvm')
  k[2:(length(args) + 1)] = args

  if(length(.lengthVars))
     k[.lengthVars] = lapply(gsub("_(.*)_length", "\\1", .lengthVars),
                             function(id)
                               substitute(length(x), list(x = as.name(id))))
  
  k[[".ee"]] = as.name('.ee')
  
  formals(fun)$.ee = .execEngine
  formals(fun) = c(formals(fun), formals(function(...){}))
  body(fun) = k
  environment(fun) = e
  fun
}


isMutableRObject =
function(var)
{
   where = sapply(var, function(x) find(x)[1])
   if(any(is.na(where)))
     stop("Cannot find variable ", if(sum(is.na(where) > 1)) "s", " ", paste(var[is.na(where)], collapse = ", "))

   var %in% ".GlobalEnv"
}

compileGlobalVariables =
function(varNames, mod, env, ir,
          mutable = sapply(varNames, isMutableRObject))
{
   ctx = getGlobalContext()
   sapply(varNames[!mutable],
           function(var) {
             val = createConstant(ir, get(var), context = ctx)   
             createGlobalVariable(var, val = val, mod, constant = TRUE)
           })

#
   #XX create variables for the mutable ones.
}


getTypeInfo =
function(fun)
{
  b = body(fun)
  if(is(b, "{"))
      e = b[[2]]
  else
      e = b

  if(is.call(e) && as.character(e[[1]]) == ".typeInfo")
      eval(e) # , globalenv())
  else
      stop("no .typeInfo() call")
}

.typeInfo = .signature =
    # We might process this as a unlisted collection and regroup them into list(returnType = ,  params = )
function(..., .types = list(...))
{
  i = sapply(.types, function(x) is(x, "externalptr") || is(x, "Type"))

  if(all(i))
      .types = list(.types[[1]], .types[-1])
  else
      .types
}

.varDecl =
function(..., .types = list(...))
{
  .types
}



compile.Integer =
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL, .useHandlers = TRUE)
{
  createIntegerConstant(call$value, type = .targetType)
}

compile.Numeric =
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL, .useHandlers = TRUE)
{
     # Prototype for casting. Needs to be more comprehensive.
   val = call$value
   if(!is.null(.targetType)) {
      if(sameType(.targetType, Int32Type))
         val = as.integer(val)
   }
   createConstant(ir, val)
}

compile.Call =
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL, .useHandlers = TRUE)
{
  # FIXME: What if $fn is not a symbol?    
  idx = match(call$fn$name, names(env$.compilerHandlers))
  if(is.na(idx)) 
     compile.call(call, env, ir, .targetType = .targetType)
  else 
     env$.compilerHandlers[[idx]](call, env, ir, .targetType = .targetType)    
}


compile.BrTerminator =
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL, .useHandlers = TRUE)    
   ir$createBr(call$dest)


compile.RetTerminator =
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL, .useHandlers = TRUE)
{
   stop("This needs to be fixed")
   Rllvm::createReturn(ir, createLoad(ir, helper$alloc_table[["._return__1"]])) # helper$alloc_table from corsair. REPLACE.
}


compile.Assign =
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL, .useHandlers = TRUE)
{
    # For dealing with assignments that are actually for Phi nodes.
if(call$write$name %in% names(env$.phiVarInstructions)) {
    .targetType = env$.types[[call$write$name]]
    i = compile(call$read, env, ir, .targetType = .targetType)
    env$.phiVarInstructions[[call$write$name]] = i
    return(i)
}

#   call = asRCall(call)
#call=node
   return(`compile.=`(call, env, ir, .targetType = .targetType))
}


compile.Symbol =
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL, .useHandlers = TRUE)
{
  v = env$getAlloc(call$name)
  if(is.null(v)) {
     v = env$.params[[ call$name ]]
     return(v)
  }

 #XXX Do we only load this if it is an AllocaInst?
 if(!is(v, "PHINode"))
    ir$createLoad( v )
 else
    v
}


compile.Replacement =
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL, .useHandlers = TRUE)
{
browser()
    e = to_r(call)
    var = call$write$basename
    if(!( var %in% names(env$.params)) )
              var = paste0(var, "_1")
    e[[2]][[2]] = as.name(var)
    
    return(`compile.=`(e, env, ir))

 
# idx = match(node$fn$name, names(cmp$.compilerHandlers))
# if (is.na(idx)) 
#    compile(node, cmp, helper)
# else 
#   cmp$.compilerHandlers[[idx]](node, cmp, helper)    
}

compile.Phi =
function(call, env, ir, ..., fun = env$.fun, name = getName(fun), .targetType = NULL, .useHandlers = TRUE)
{
#XXX Fix    
    node = call
    cmp = env
    helper = ir
    types = env$.types
    
#XXXX
# Don't insert phis for shadowed globals.
#  if ( any(is_global(node$read)) )
#    return (NULL)


  phiName = node$write$name
  
  type = types[[node$write$name]]
  numIncoming = length(node$blocks)
  phi = Rllvm::createPhi(helper, type, numIncoming, id = node$write$name)
# Add the incoming. We have them in the node$blocks and in node$read

# We may not have created all of the incoming values for the phi nodes at this point
# as the order of the CFG may put put creation of the incoming after the Phi node 
# into which it comes.
# So we have to make an incomplete Phi node and add its identity and what it is waiting
# for (i.e. the variable in the Assignment) and when we process that we check to see if
# we need to add it to any of the incomplete Phi nodes.

  
  mapply(function(var, block) {
           val = cmp$.phiVarInstructions[[ var$name ]]

           if(is.null(val)) {
                # so the intruction has not been processed yet.
                # We create a dummy Value and then arrange to replace
                # it with the actual instruction at the end of the module/
                # routine construction. This is the purpose of
                # the llvm function replaceAllUsesWith()
                # This approach is what llvm uses itself when we create
                # the c++ API code from a .ll file. i.e., we are copying
                # that directly.
             val = .Call("R_createFwdRef_for_phi", type)
             cmp$.phiForwardRefs[[ var$name ]] = val
           }

           addIncoming(phi, val, block)
         }, node$read, cmp$blocks[node$blocks])

  if(node$write$name %in% names(cmp$.phiVarInstructions)) 
     cmp$.phiVarInstructions[[ phiName ]] <- phi
  else
     cmp$.allocVars[[ phiName ]] <- phi
  
  phi
}






#########################

#??? Kill off
XXXX.assignHandler =
function(call, env, ir)
{
   args = call[-1]

   val = compile(args[[2]], env, ir)

          # CreateLocalVariable, with a reference in the
          # environment, and then CreateStore of the value.
   checkArgs(args, list(c('character', 'symbol'), 'ANY'), '<-')
     # XXX may not be a variable
   if(is.name(var))
      var <- as.character(args[1])
   
   val <- args[[2]]
   if (is.na(findVar(var, env))) {
        # Create new local store, TODO remove the type here and infer it
     type = getDataType(val, env)
     assign(var, createFunctionVariable(type, var, env, ir), envir = env) ## Todo fix type
     env$.types[[var]] = type
   }
   
   ref <- get(var, envir = env)

      # Now, create store. TODO: how does this work *without*
      # constants? Where is evaluation handled... probably not
      # here?
   createStore(ir, val, ref)
 }

