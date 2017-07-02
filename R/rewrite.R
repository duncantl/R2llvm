#
# Several of these may belong in rstatic. Need to figure out what the scope of each package is. And also the overlap with CodeAnalysis.

#
# To test
# library(rstatic); source("~/NickUThesis/rstatic/R/ASTTraverse.R"); source("../R/rewrite.R");
# a = to_astq(x[i - 1]); astTraverse(a, makeIntegerLiterals)
# a = to_astq(x[1:n]); astTraverse(a, makeIntegerLiterals)
# a = to_astq(x[1:10]); astTraverse(a, makeIntegerLiterals)

#
# a = to_astq(runif(1, .1)); astTraverse(a, rewriteRUnif)
# a = to_astq(runif(1, .1, .7)); astTraverse(a, rewriteRUnif)
# a = to_astq(runif(1, max = .7)); astTraverse(a, rewriteRUnif)


rewriteAST =
function(ast, ..., functionRewrites = getFunctionRewrites())
{
  astTraverse(ast, makeIntegerLiterals)
  astTraverse(ast, rewriteListCreate)
  astTraverse(ast, rewriteFor)
  lapply(functionRewrites, function(f) astTraverse(ast, f))
  astTraverse(ast, rewriteProtect)
  ctr = rewriteCountProtects()
  astTraverse(ast, ctr)
  # Too simple. Just temporary. Need to take account of conditional allocs.
  # Or we just reset the stack at the end. Or use live analysis to identify when we can unprotect() an object.
  numProtects = environment(ctr)$count
  if(numProtects > 0) {
      e = ast$body$body
      u = to_astq(Rf_unprotect(1L))
      u$args[[1]]$value = numProtects
      insertNode(ast$body, e[[ length(e) ]], u, before = TRUE)
  }
}


if(FALSE) 
rewriteSubsetLiterals =
function(node, ...)
{
   if(is(node, "Call") && node$fn$name %in% c("[", "[[")) {
      sapply(node$args, makeIntegerLiterals)
   } else if(is(node, "Replacement") && node$fn %in% c("[", "[[")) {
       a = node$args[ - length(node$args)]
   }
}

makeIntegerLiterals =
    #
    # Process elements in the AST and convert those with Numeric literals
    # when we know they should be treated as integers
    # e.g. x[1] and 1:n
function(node)
{
    # Do we work recursively or do we look up at the parent.
 if(is(node, "Numeric") && (isInSubsetCall(node) || isInColonCall(node))) {
    replaceNode(node$parent, node, Integer$new(as.integer(node$value)), error = FALSE)
 }
    
}

isInColonCall =
function(node)
{
  is(node$parent, "Call") && node$parent$fn$name == ":"
}

isInSubsetCall =
function(x)
{
  p = x$parent
  while(!is.null(p)) {
     if(is(p, "Call") && p$fn$name %in% c("[", "[["))
         return(TRUE)
     p = p$parent
  }
  FALSE
}


rewriteRUnif =
function(node, ...)
{
  if(is(node, "Call") && node$fn$name == "runif") {
      node$fn$basename = "Rf_runif"
      params = list(min = Numeric$new(0), max = Numeric$new(1))
      a = node$args[-1]
      if(length(a) > 0) {
          if(length(names(a)) > 0) {
             m = match.call(runif, to_r(node))
             j = match(names(m), names(params))
             params[j[!is.na(j)]] =  a
          } else
             params[seq(along = a)] = a
        
      }
      node$args = unname(params)
  }
}




rewriteProtect =
function(node, ...)
{
  if(is(node, "Assign") && isRAlloc(node$read)) {
     k = rstatic::Call$new(Symbol$new("Rf_protect"), list(node$write$copy()))
     insertNode(node$parent, node, k, FALSE)
  }
}

rewriteListCreate =
function(node, ...)
{
  if(is(node, "Return") && length(node$args) > 0 && is(node$args[[1]], "Call") && node$args[[1]]$fn$name == "list"
        && length(node$args[[1]]$args)) {
#      browser()
      k = node$args[[1]]
      a = to_astq(.return <- mkList())  # Rf_allocVector(19L, n))
      a$read$args[[1]] = Integer$new(length(k$args))
      set.els = mapply(function(i, x) {
#                        e = to_astq(.return[[i]] <- x)
#                        e$read$args[[2]] = Integer$new(i)
#                        e$read$args[[3]] = x

  e = to_astq(SET_VECTOR_ELT(x, i, val))
  e$args[[1]] = Symbol$new(".return")
  e$args[[2]] = Integer$new(i-1L)
  e$args[[3]] = x
                         e
                       }, seq(along = k$args), k$args)
      r = rstatic::Return$new(list(Symbol$new(".return")))
# Set the names      
      replaceNode(node$parent, node, c(a, to_astq(Rf_protect(.return)), set.els, r))
  }
    
  else if(is(node, "Assign") && isRAlloc(node$read) && node$read$fn$name == "list") {
      browser()
#     k = rstatic::Call$new(Symbol$new("Rf_protect"), list(node$write$copy()))
#     insertNode(node$parent, node, k, FALSE)
  }
}


rewriteCountProtects =
function()
{
  count = 0L
  function(node, ...) {
     if(is(node, "Call") && node$fn$name == "Rf_protect")
         count <<- count + 1L
  }
}


isRAlloc =
function(node)
{
  is(node, "Call") && node$fn$name %in% c('numeric', 'logical', 'integer', 'vector', 'list')
  # Or a call to a function that returns an R object.
}



FunctionRewriteDefault =
list(runif = rewriteRUnif)    

getFunctionRewrites =
function(..., .default = FunctionRewriteDefault)
{
   o = list(...)
   if(length(o))
     .default[names(o)] = o
   
   .default
       
}


rewriteFor =
function(node, error = TRUE, ...)
{
  if(is(node, "For"))   {
      # Process the body first for nested loops.
#print(node)
    # recursive processing for nested loops.
#browser()    
 astTraverse(node$body, rewriteFor, error = TRUE)

     # cond = substitute(i < n, list(i = as.name(i)
     #XXX cover more situations of course e.g. n:2 and i >= 2,  1:length(x)
    cond = Call$new("<=", list(node$ivar$copy(), node$iter$args[[2]]$copy()))
       # might want to write Call(++, i) or Call(intIncr, i) so the compiler could recognize this.
    inc = Assign$new(node$ivar$copy(), Call$new("+", list(node$ivar, Integer$new(1L))))
    o = b = if(is.list(node$body))
               node$body
            else
               node$body$copy()

   
    if(!is(b, "Brace"))
        b = rstatic::Brace$new(list(b))

    b$body = append(b$body, inc)
    whileLoop = rstatic::While$new(cond, b)
    init = rstatic::Assign$new(node$ivar$copy(), node$iter$args[[1]]$copy())

#debug(replaceNode)
    replaceNode(node$parent, node, list(init, whileLoop), error = TRUE)
#undebug(replaceNode)
   }
  
  TRUE
}




if(FALSE) {
f =
function(n)
{
  total = 0L
  for(i in 2L:n) # with and without { around body.
      total = total + i
  return(total)
}
ast = rstatic::to_ast(f)
z = astTraverse(ast, function(x) print(class(x)))
z = astTraverse(ast, rewriteFor)
ast
}

