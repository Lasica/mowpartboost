set.seed(123)

vn <- sample(1:100, 100)
vs <- sample(colors(), 100)

mn <- matrix(vn, nrow=25, ncol=4)
ms <- matrix(vs, nrow=25, ncol=4)
d <- cbind(as.data.frame(mn), as.data.frame(ms))
names(d) <- c(paste("n", 1:4, sep=""), paste("s", 1:4, sep=""))

####

n1s <- d$n1^2
n12d <- d$n1-d$n2
n12ssd <- sum((d$n1-d$n2)^2)
n1c <- ifelse(d$n1<20, 20, ifelse(d$n1>80, 80, d$n1))
s1s <- substr(d$s1, 1, 4)
s12 <- paste(d$s1, d$s2, sep=".")

####

is.prime1.simple <- function(n) n==2 || all(n%%(2:(n-1))>0)
is.prime.simple <- Vectorize(is.prime1.simple)

sapply(d$n1, is.prime1.simple)
is.prime.simple(d$n1)
sapply(d[,1:4], is.prime.simple)
sum(sapply(d[,1:4], is.prime.simple))
lapply(d[,1:4], is.prime.simple)
sapply(lapply(d[,1:4], is.prime.simple), sum)

####

is.prime1.loop <- function(n)
{
  if (n<=3)
    return(n>1)
  for (f in 2:(n-1))
    if (n%%f==0)
      return(FALSE)
  TRUE
}

is.prime.loop <- Vectorize(is.prime1.loop)

is.prime1.betterloop <- function(n)
{
  if (n<=3)
    return(n>1)
  if (n%%2==0 || n%%3==0)
    return(FALSE)
  if (n<25)
    return(TRUE)
  for (f in seq(5, as.integer(sqrt(n)), 6))
    if (n%%f==0 || n%%(f+2)==0)
      return(FALSE)
  TRUE
}

is.prime.betterloop <- Vectorize(is.prime1.betterloop)

system.time(sapply(1:1000, is.prime.simple))
system.time(sapply(1:1000, is.prime.loop))
system.time(sapply(1:1000, is.prime.betterloop))

####

divisors <- function(n) (2:(n-1))[n%%(2:(n-1))==0]

lapply(d$n1, divisors)
sapply(lapply(d$n1, divisors), length)

####

apply(mn, 1, mean)
rowMeans(mn)
apply(mn, 2, mean)
colMeans(mn)

apply(ms, 1, function(v) sum(nchar(v)))
apply(ms, 2, function(v) sum(nchar(v)))

mapply(function(n, s) paste(s, n, sep="."), d$n1, d$s1)

sapply(d, function(v) if (is.numeric(v)) mean(v) else names(table(v))[which.max(table(v))])

t(sapply(1:nrow(d), function(i) c(mn=mean(unlist(d[i,1:4])), sn=sd(unlist(d[i,1:4])))))
t(apply(d[,1:4], 1, function(r) c(mn=mean(r), sn=sd(r))))

outer(d$n1, d$n2)
outer(d$s1, d$s2, function(s1, s2) nchar(as.character(s1))+nchar(as.character(s2)))
#outer(1:4, 1:4, function(i, j) unlist(d[,i]) %*% unlist(d[,j]))
outer(1:4, 1:4, Vectorize(function(i, j) unlist(d[,i]) %*% unlist(d[,j])))
