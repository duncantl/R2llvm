library(rstatic)
rw2d = function(n = 100L) {
  # FIXME: xpos = ypos = numeric(n)
  xpos = integer(n)
  ypos = integer(n) # xpos
  for(i in 2L:n) {
    # Decide whether we are moving horizontally or vertically.
    if (Rf_runif(0, 1) > .5)
      delta = 1L
    else
      delta = -1L

    if (Rf_runif(0, 1) > .5) {
      xpos[i] = xpos[i-1] + delta
      ypos[i] = ypos[i-1]
    }
    else {
      xpos[i] = xpos[i-1]
      ypos[i] = ypos[i-1] + delta
    }
  }
  list(x = xpos, y = ypos)
}

source("../R/rewrite.R")

ast = to_ast(rw2d)
rewriteAST(ast)

if(FALSE) {
astTraverse(ast, rewriteListCreate)
astTraverse(ast, rewriteProtect)
ctr = rewriteCountProtects()
astTraverse(ast, ctr)
print(environment(ctr)$count)


astTraverse(ast, rewriteRUnif)
astTraverse(ast, rewriteFor)

}
library(R2llvm)

cfg = rstatic::to_cfg(ast) 
tt = RTypeInference::infer_types(cfg)
RTypeInference::return_type(tt)
fc = compileFunction(ast, cfg, tt, name = "rw2d")


