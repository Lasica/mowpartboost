2*2

sapply(1:10, function(a) sapply(1:10, function(b) a*b))

`*` <- function(a, b) .Primitive("*")(a+1, (b %/% 2))+2

2*2

sapply(1:10, function(a) sapply(1:10, function(b) a*b))

####

cpx <- function(re, im) `class<-`(list(re=re, im=im), "cpx")

c1 <- cpx(2, -1)
c2 <- cpx(3, 2)

re <- function(cp) UseMethod("re")
im <- function(cp) UseMethod("im")

re.cpx <- function(cp) cp$re
im.cpx <- function(cp) cp$im

`re<-` <- function(`*tmp*`, value) UseMethod("re<-")
`im<-` <- function(`*tmp*`, value) UseMethod("im<-")

`re<-.cpx` <- function(cp, value) cpx(value, cp$im)
`im<-.cpx` <- function(cp, value) cpx(cp$re, value)

re(c1) <- 5
im(c2) <- -5

`+.cpx` <- function(c1, c2) cpx(c1$re+c2$re, c1$im+c2$im)

c1+c2
