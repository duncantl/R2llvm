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

    prev = i-1L
    if (Rf_runif(0, 1) > .5) {
      xpos[i] = xpos[prev] + delta
      ypos[i] = ypos[prev]
    }
    else {
      xpos[i] = xpos[prev]
      ypos[i] = ypos[prev] + delta
    }
  }
  list(x = xpos, y = ypos)
}

#cfg = rstatic::to_cfg(rw2d) # infer_types() doesn't work if use ssa = FALSE)
#tt = RTypeInference::infer_types(cfg)

library(R2llvm)

ast = rstatic::to_ast(rw2d)
R2llvm:::rewriteAST(ast)
cfg = rstatic::to_cfg(ast) 
types = RTypeInference::infer_types(cfg, init = list(prev = typesys::IntegerType()))

fc = compileFunction(ast, cfg, types, name = "rw2d", .rewriteAST = FALSE)
#fc = compileFunction(rw2d)
