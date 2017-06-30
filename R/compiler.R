
compiler =
function(.compilerHandlers = getCompilerHandlers(),
         NAs = FALSE,
         .builtInRoutines = getBuiltInRoutines(),
         .functionInfo = list(),
         structInfo = list(),
         .zeroBased = TRUE,
         .integerLiterals = TRUE,
         .useFloat = FALSE,
         .debug = TRUE, .assert = TRUE,
         .addSymbolMetaData = TRUE,
         .CallableRFunctions = list(),
         irbuilder = NULL,         
         module = NULL,
         block = NULL,
         fun = NULL,
         types = list(),
         ...,
         compiler = makeCompileEnv())
{
    addToCompiler(compiler,
                    .compilerHandlers = .compilerHandlers,
                    .builtInRoutines = .builtInRoutines,
                    .functionInfo = .functionInfo,
                    .CallableRFunctions = .CallableRFunctions
                  )

    compiler$.NAs = NAs
    compiler$.structInfo = structInfo
    compiler$.loopDepth = 0L
    compiler$.zeroBased = .zeroBased
    compiler$.integerLiterals = .integerLiterals
    compiler$.useFloat = .useFloat
    compiler$.debug = .debug
    compiler$.assertFunctions = .assert
    compiler$.addSymbolMetaData = .addSymbolMetaData

    compiler$.phiForwardRefs = list()
    
    compiler$.SetCallFuns = list()
    compiler$.loopStack = character()

    compiler$.irbuilder = irbuilder
    if(!is.null(irbuilder) && is.null(block) )
        block = getInsertBlock(irbuilder)

    if(!is.null(block) && is.null(fun) )
        fun = as(block, "Function")
    compiler$.fun = fun

    if(!is.null(fun) && is.null(module))
        module = as(fun, "Module")
    compiler$.module = module

    compiler$.entryBlock = block

    compiler$.types = types
    
    compiler
}

addToCompiler =
function(compiler, ..., merge = TRUE)
{
  args = list(...)
  vars = ls(compiler, all = TRUE)
  mapply(function(id, val) {
           if(length(val)) {
               if(merge && id %in% vars) {
                   old = get(id, compiler)
                   old[names(val)] = val
                   val = old
               }
               assign(id, val, compiler)
           } else if(!merge)
               assign(id, val, compiler)
         }, names(args), args)
  
  compiler
}




mkCompiler =
function(fun = NULL, curBlock = NULL, .types = list(), mod = NULL,
         .compilerHandlers = getCompilerHandlers(),
         .builtInRoutines = getBuiltInRoutines(),         
         ...)
{
    if(missing(mod) && !missing(fun))
        mod = as(fun, "Module")
    
    nenv = makeCompileEnv()
    nenv$.entryBlock = curBlock
    nenv$.types = .types
    nenv$.module = mod
    nenv$.compilerHandlers = .compilerHandlers
    nenv$.loopDepth = 0L
    nenv$.SetCallFuns = list()
    nenv$.loopStack = character()
    nenv$.builtInRoutines = .builtInRoutines    
    
    opts = list(...)
    mapply(function(name, val)
             assign(name, val, nenv),
           names(opts), opts)
    
    nenv
}


makeCompileEnv =
function()
{
   nenv <- new.env( parent = emptyenv())
#   nenv$.continueBlock = list()
#   nenv$.nextBlock = list()

   nenv$declFunction = function(name) 
        declareFunction(nenv$.builtInRoutines[[name]], name, nenv$.module)

   nenv$.funCalls = list()
   nenv$.allocVars = list()
   
   nenv$addCallInfo = function(name, retType = NULL, types = NULL) {
        i = length(nenv$.funCalls)
        nenv$.funCalls[[i + 1L]] <<- list(name, returnType = retType, params = types)
        names(nenv$.funCalls)[i + 1L] <<- name
        TRUE
   }

   nenv$newAlloc = function(var, inst)
                      nenv$.allocVars[[ var ]] <<- inst
   nenv$getAlloc = function(var)
                        nenv$.allocVars[[ var ]]
   nenv
}
