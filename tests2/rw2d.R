rw2d = function(n = 100L) {
  # FIXME: xpos = ypos = numeric(n)
  xpos = integer(n)
  ypos = integer(n) # xpos
  for(i in 2L:n) {
    # Decide whether we are moving horizontally or vertically.
    if (runif(1) > .5)
      delta = 1L
    else
      delta = -1L

    if (runif(1) > .5) {
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

cfg = rstatic::to_cfg(rw2d) # infer_types() doesn't work if use ssa = FALSE)
tt = RTypeInference::infer_types(cfg)

