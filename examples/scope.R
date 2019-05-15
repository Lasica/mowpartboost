set.seed(123)

vn <- sample(1:100, 100)
mn <- matrix(vn, nrow=25, ncol=4)

nneighb <- function(m)
{
  nn <- rep(NA, nrow(m))
  sapply(1:nrow(m),
         function(i)
         {
           di <- function(r) m[i,]%*%r/(sqrt(r%*%r)*sqrt(m[i,]%*%m[i,]))
           nn[i] <<- which.max(apply(m[-i,], 1, di))
         })
  nn
}

nneighb(mn)


count.events <- function(n0=0)
{
  n <- n0
  list(reset=function(n0) n <<- n0, count=function() n, event=function() n <<- n+1)
}

ce <- count.events()

ce$event()
ce$event()
ce$event()
ce$count()
ce$reset(1)
ce$count()
