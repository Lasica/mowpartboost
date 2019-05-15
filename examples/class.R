tree <- `class<-`(list(root=1,
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
                                        right=NULL))),
                  "listree")

####

print.listree <- function(lt, ind=0)
{
  if (!is.null(lt))
  {
    cat(rep("| ", ind), lt$root, "\n", sep="")
    print.listree(lt$left, ind+1)
    print.listree(lt$right, ind+1)
  }
}

print(tree)

####

sum.listree <- function(lt, na.rm=FALSE)
{
  if (is.null(lt))
    0
  else
    lt$root+sum.listree(lt$left)+sum.listree(lt$right)
}

sum(tree)

####

depth.listree <- function(lt)
{
  if (is.null(lt))
    0
  else
    1+max(depth.listree(lt$left), depth.listree(lt$right))
}

depth <- function(obj) UseMethod("depth")

depth(tree)
