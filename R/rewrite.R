
rewriteAST =
function(ast, ..., functionRewrites = getFunctionRewrites())
{
  astTraverse(ast, rewriteListCreate)
  astTraverse(ast, rewriteFor)
  lapply(functionRewrites, function(f) astTraverse(ast, f))
  astTraverse(ast, rewriteProtect)
  ctr = rewriteCountProtects()
  astTraverse(ast, ctr)
  # Too simple. Just temporary
  numProtects = environment(ctr)$count
  if(numProtects > 0) {
      e = ast$body$body
      u = to_astq(Rf_unprotect(1L))
      u$args[[1]]$value = numProtects
      insertNode(ast$body, e[[ length(e) ]], u, before = TRUE)
  }
}


rewriteRUnif =
function(node, ...)
{
  if(is(node, "Call") && node$fn$name == "runif") {
      node$fn$name = "Rf_runif"
      node$args = list(Numeric$new(0), Numeric$new(1))
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
