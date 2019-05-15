set.seed(123)

vn <- sample(1:100, 100)

bs.mean1 <- function(v, nb=10000)
{
  sapply(1:nb,
         function(b)
         mean(sample(v, length(v), replace=TRUE)))

}

bs.mean <- function(v, m100=bs.mean1(v, 100), m1000=bs.mean1(v, 1000),
                    m10000=bs.mean1(v, 10000), m100000=bs.mean1(v, 100000),
                    nb=100)
{
  if (nb<=100)
    mean(m100)
  else if (nb<=1000)
    mean(m1000)
  else if (nb<=10000)
    mean(m10000)
  else
    mean(m100000)
}

var(bs.mean1(vn, 10000))

sapply(10^(2:5), function(nb) system.time(bs.mean(vn, nb=nb)))
