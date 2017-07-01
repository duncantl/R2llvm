library(R2llvm)

#debug(compileFunction)
#RTypeInference::infer_types(f)
f = function(x) 1L + x
cfg = rstatic::to_cfg(f) # , ssa = FALSE)

# Need to specify the type as we cannot narrow it down otherwise.
# Although of course we could.
types = RTypeInference::infer_types(cfg, init = list(x = typesys::IntegerType()))
fc = compileFunction(f, cfg = cfg, types = types, optimize = FALSE)

showModule(fc)
stopifnot(identical(.llvm(fc, 10), 11L))

# Now real type as input and add  numeric 1 so no casting necessary
f = function(x) 1 + x
cfg = rstatic::to_cfg(f) #, ssa = FALSE)
types = RTypeInference::infer_types(cfg, init = list(x = typesys::RealType()))
fc = compileFunction(f, cfg = cfg, types = types, optimize = FALSE)
stopifnot(identical(.llvm(fc, 2.5), 3.5))


# See cast.R for casting the 1L to Double

