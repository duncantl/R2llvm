f =
function(x)
{
   if(x > 0)
       20
   else
       -1
}

f1 =
function(x)
{
   if(x > 0)
       return(20)
   else
       return(-1)
}

f2 =
function(x)
{
 return(if(x > 0) 20 else -1)
}


library(rstatic)
to_cfg(f)
to_cfg(f1)
to_cfg(f2)
