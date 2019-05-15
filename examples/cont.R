## TODO: produce a more useful example

callCC(function (exitfun)
       {
         f <- function(v, nc=1)
         {
           if (sample(1:100, 1)==v)
             exitfun(v)
           print(nc)
           f(v, nc+1)
         }
         f(50)
       })



callCC(function (exitfun)
       {
         shot <- function(vec, v)
         { if (vec[i <- sample(1:length(vec), 1)]==v)
             exitfun(list(vec=vec, i=i))
           shot(vec[-i], v) 
         }
         shot(1:100, 50)
       })
