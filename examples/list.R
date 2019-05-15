set.seed(123)

vn <- sample(1:100, 100)
vs <- sample(colors(), 100)

####

ln <- as.list(vn)
ls <- as.list(vs)

ln[ln>10 & ln<30]
which(ln>10 & ln<30)
sum(ln>10 & ln<30)

ls[ls>="lemon" & ls<="orange"]
which(ls>="lemon" & ls<="orange")
sum(ls>="lemon" & ls<="orange")

ls[ln>10 & ln<30]
ln[ls>="lemon" & ls<="orange"]

####

names(ln) <- names(ls) <- paste("li", 1:100, sep="")

ln[5]
ln[[5]]
ln[["li5"]]
ln$li5

####

ln.lt <- lapply(seq(10, 100, 10), function(v) ln[ln<=v])

####

tree <- list(root=1,
             left=list(root=2,
                       left=list(root=4,
                                 left=list(root=8,
                                           left=NULL,
                                           right=NULL),
                                 right=list(root=9,
                                            left=NULL,
                                            right=NULL)),
                       right=list(root=5,
                                  left=list(root=10,
                                            left=NULL,
                                            right=NULL),
                                  right=list(root=11,
                                             left=NULL,
                                             right=NULL))),
             right=list(root=3,
                        left=list(root=6,
                                  left=NULL,
                                  right=NULL),
                        right=list(root=7,
                                   left=NULL,
                                   right=NULL)))
