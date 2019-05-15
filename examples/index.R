set.seed(123)

vn <- sample(1:100, 100)
vs <- sample(colors(), 100)

####

vn[vn>10 & vn<30]
which(vn>10 & vn<30)
sum(vn>10 & vn<30)

vs[vs>="lemon" & vs<="orange"]
which(vs>="lemon" & vs<="orange")
sum(vs>="lemon" & vs<="orange")

vs[vn>10 & vn<30]
vn[vs>="lemon" & vs<="orange"]

vs[grep("light", vs)]
grep("light", vs)
length(grep("light", vs))

####

mn <- matrix(vn, nrow=25, ncol=4)

mn[1:10,]
mn[,2:3]
mn[1:10, 2:3]
mn[1:10, c(1, 4)]
mn[-(11:25), -(2:3)]

####

ms <- matrix(vs, nrow=25, ncol=4)
d <- cbind(as.data.frame(mn), as.data.frame(ms))
names(d) <- c(paste("n", 1:4, sep=""), paste("s", 1:4, sep=""))

d[,2]
d$n2
d[,"n2"]
d[["n2"]]

d[d[,2]>50,]

d[d[,2]>50,5:8]
d[d$n2>50,c("s1", "s2", "s3", "s4")]
d[d$n2>50,setdiff(names(d),c("n1", "n2", "n3", "n4"))]

l2 <- paste(letters, 2, sep="")
d[d$n2>50,l2[l2 %in% names(d)]]
d[d$n2>50,c("n2", "s2")]
