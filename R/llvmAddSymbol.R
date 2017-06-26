# This is a workaround for R no longer making the routines in the interpreter accessible
# via getNativeSymbolInf().
# We have implemented a call to dlopen() and dlsym().
setAs("character", "NativeSymbol",
      function(from) {
       tryCatch(getNativeSymbolInfo(from)$address,
                error = function(e) {
                   structure(.Call("R_getRSymbol", from), class = "NativeSymbol")
                })
      })

