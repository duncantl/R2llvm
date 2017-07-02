f =
function()
{
   total = 0L

   # Do w/o the {}
   for(i in 1:10) 
       for(j in 100:200) 
           total = total + j
       
   
   return(total)
}

g =
function()
{
   total = 0L

   # Do w/o the {}
   for(i in 1:10) {
       for(j in 100:200) {
           total = total + j
       }
   }
   
   return(total)
}


h =
function()
{
   total = 0L

   # Do w/o the {}
   for(i in 1:10) {
       for(j in 100:200) {
         d = 0
         for(k in 1:p) {
             d = d + abs(x[i,k] - x[j, k])
         }
       }
   }
   
   return(total)
}



library(rstatic); source("~/NickUThesis/rstatic/R/ASTTraverse.R"); source("../R/rewrite.R");

o = lapply(list(f, g, h), function(f) {
                            a = rstatic::to_ast(f)
                            rewriteAST(a)
                            a
                          })



