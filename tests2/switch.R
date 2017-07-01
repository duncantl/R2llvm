f =
function(i)
{
   x = switch(i,
            '2' = 12,
            '3' = i * 1.5,
            '8' = i/2)

   x
}

library(R2llvm)
fc = compileFunction(f)

